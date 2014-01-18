
# DES.R:  R routines for discrete-event simulation (DES), with an example

# matrix version; data frame allows character event types, but much too slow

# all data is stored in an R environment variable that will be referrred
# to as simlist below

# the simlist will consist of the following components:
#
#       currtime:  current simulated time
#       evnts:  the events list, a matrix
#       reactevent:  event handler, user-supplied; creates new
#                    events upon the occurrence of an old one;
#                    e.g. job arrival triggers either start of 
#                    service for the job or queuing it; call form is
#                    reactevent(evnt,simlist)
#       dbg:  if TRUE, will print evnts above after each event
#             scheduling action, and enter R browser for single-stepping
#             etc.

# the application code can add further application-specific data to
# simlist, e.g. total job queuing time 

# each event will be represented by a matrix row consisting of: 
# 
#    occurrence time
#    event type (user-defined numeric code)
#
# and application-specific information, if any

# library functions (do not alter):
# 
#       newsim:  create a new simlist
#       insevnt:  insert a new event into evnts in the simlist
#       schedevnt:  schedule a new event (determine its occurrence time
#                   and call insevnt())
#       getnextevnt:  pulls the earliest event from the event set,
#                     process it, and update the current simulated
#                     time
#       mainloop:  as the name implies
#       appendtofcfsqueue:  append job to a FCFS queue
#       delfcfsqueue:  delete head of a FCFS queue

# outline of a typical application:

#    mysim <- newsim()    create the simlist
#    set reactevent in mysim
#    set application-specific variables in mysim, if any
#    set the first event in mysim$evnts
#    mainloop(mysim,mysimtimelim)
#    print results

# create a simlist, which will be the return value, an R environment
newsim <- function(dbg=F) {
  simlist <- new.env()  # new environment: like a pointer
  simlist$currtime <- 0.0  # current simulated time
  simlist$evnts <- NULL  # event set
  simlist$dbg <- dbg  # printing flag
  simlist
}

# insert event evnt into evnts in simlist
insevnt <- function(evnt,simlist) {
  # if the event set is empty, set it to consist of evnt and return
  if (is.null(simlist$evnts)) {
    simlist$evnts <- matrix(evnt,nrow=1)
    return()
  }
  # otherwise, find insertion point
  inspt <- binsearch(simlist$evnts[,1],evnt[1])
  # now "insert," by reconstructing the matrix; we find what portion of
  # the current matrix should come before evnt and what portion should 
  # come after it, then string everything together
  before <- if (inspt == 1) NULL else simlist$evnts[1:(inspt-1),]
  nr <- nrow(simlist$evnts)
  after <- if (inspt <= nr) simlist$evnts[inspt:nr,] else NULL  
  simlist$evnts <- rbind(before,evnt,after)  
  rownames(simlist$evnts) <- NULL
}

# schedule new event in evnts in simlist; evnttime is the time at
# which the event is to occur; evnttype is the event type; appdata is
# a vector of numerical application-specific data
schedevnt <- function(evnttime,evnttype,simlist,appdata=NULL) {
  evnt <- c(evnttime,evnttype,appdata)
  insevnt(evnt,simlist)  
}

# start to process next event (second half done by application
# programmer via call to reactevnt() from mainloop())
getnextevnt <- function(simlist) {
  head <- simlist$evnts[1,]
  # delete head
  if (nrow(simlist$evnts) == 1) simlist$evnts <- NULL else 
    simlist$evnts <- simlist$evnts[-1,,drop=F]  
  return(head)
}

# main loop of the simulation
mainloop <- function(simlist,simtimelim) {
  while(simlist$currtime < simtimelim) {
    head <- getnextevnt(simlist)  
    # update current simulated time
    simlist$currtime <- head[1]  
    # process this event (programmer-supplied ftn)
    simlist$reactevent(head,simlist)  
    if (simlist$dbg) {
      print("event occurred:")
      print(head)
      print("events list now")
      print(simlist$evnts)
      browser()
    }
  }
}

# binary search of insertion point of y in the sorted vector x; returns
# the position in x before which y should be inserted, with the value
# length(x)+1 if y is larger than x[length(x)]; this could be replaced
# by faster C code
binsearch <- function(x,y) {
  n <- length(x)
  lo <- 1
  hi <- n
  while(lo+1 < hi) {
    mid <- floor((lo+hi)/2)
    if (y == x[mid]) return(mid)
    if (y < x[mid]) hi <- mid else lo <- mid
  }
  if (y <= x[lo]) return(lo)
  if (y < x[hi]) return(hi)
  return(hi+1)
}

# appendtofcfsqueue() and delfcfsqueuehead() below assume the
# application code has one or more queues, each queue stored as a
# list-of-lists, with each individual list being the information for one
# queued job; note that one must initialize the list-of-lists as NULL 

# appends jobtoqueue to the given queue, assumed of the above form;
# the new, longer list is returned
appendtofcfsqueue <- function(queue,jobtoqueue) {
  lng <- length(queue)
  queue[[lng+1]] <- jobtoqueue
  queue
}

# deletes head of queue; assumes list-of-lists structure as decribed
# above; returns the head and new queue
delfcfsqueuehead <- function(queue) {
  qhead <- queue[[1]]
  newqueue <- queue[-1]
  # careful!--an empty list is not NULL  
  if (length(queue) == 1) newqueue <- NULL
  list(qhead=qhead,newqueue=newqueue)
}

# test; M/M/1 queue--exponential ("Markov" job interarrivals,
# exponential service times, 1 server
mm1 <- function(meaninterarrv,meansrv,timelim,dbg=F) {
  simlist <- newsim(dbg)
  simlist$reactevent <- mm1react  # defined below
  simlist$arrvrate <- 1 / meaninterarrv
  simlist$srvrate <- 1 / meansrv
  simlist$totjobs <- 0
  simlist$totwait <- 0.0
  simlist$queue <- NULL
  simlist$srvrbusy <- F
  # defining job numbers is good practice, always invaluable during
  # debugging
  simlist$jobnum <- 0
  # event type codes: 1 for arrival, 2 for service completion;
  # set up first event, including info on this job's arrival time for
  # later use in finding mean wait until job done
  timeto1starrival <- rexp(1,simlist$arrvrate)
  jobnum <- simlist$jobnum + 1
  simlist$jobnum <- jobnum
  schedevnt(timeto1starrival,1,simlist,c(timeto1starrival,jobnum))
  mainloop(simlist,timelim)
  # should print out 1 / (srvrate - arrvrate)
  cat("mean wait:  ")
  print(simlist$totwait / simlist$totjobs)
}

# what new events are triggered by the occurrence of an old one?
# M/M/S/∞ 表示输入过程是Poisson流, 服务时间服从负
# 指数分布,  系统有S个服务台平行服务, 系统容量为无穷的
# 等待制排队系统.
# 即顾客到达系统的相继到达时间间隔独立，且服从参数
# 为λ的负指数分布(即输入过程为Poisson过程), 服务台
# 的服务时间也独立同分布,  且服从参数为μ的负指数分
# 布，而且系统空间无限，允许永远排队.
mm1react <- function(evnt,simlist) {
  etype <- evnt[2]
  if (etype == 1) {  # job arrival
    # schedule next arrival
    timeofnextarrival <- simlist$currtime + rexp(1,simlist$arrvrate)
    jobnum <- simlist$jobnum + 1
    simlist$jobnum <- jobnum
    schedevnt(timeofnextarrival,1,simlist,c(timeofnextarrival,jobnum))
    # start newly-arrived job or queue it
    if (!simlist$srvrbusy) {  # start job service
      simlist$srvrbusy <- T
      srvduration <- rexp(1,simlist$srvrate)
      schedevnt(simlist$currtime+srvduration,2,simlist,evnt[3:4])
    } else {  # add to queue
      simlist$queue <- appendtofcfsqueue(simlist$queue,evnt)
    }
  } else if (etype == 2) {  # job completion
    # bookkeeping
    simlist$totjobs <- simlist$totjobs + 1
    simlist$totwait <- simlist$totwait + simlist$currtime - evnt[3]
    simlist$srvrbusy <- F
    # check queue for waiting jobs
    if (!is.null(simlist$queue)) {
      tmp <- delfcfsqueuehead(simlist$queue)
      job <- tmp$qhead
      simlist$queue <- tmp$newqueue
      # start job service
      simlist$srvrbusy <- T
      srvduration <- rexp(1,simlist$srvrate)
      schedevnt(simlist$currtime+srvduration,2,simlist,job[3:4])
    }
  } 
}

