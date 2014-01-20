
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

# create a simlist (simulation list), which will be the return value, an R environment
# library functions (do not alter)
newsim <- function(dbg=F) {
  simlist <- new.env()  # new environment 宏观控制: like a pointer
  simlist$currtime <- 0.0  # current simulated time
  simlist$evnts <- NULL  # event set
  simlist$dbg <- dbg  # printing flag
  simlist
}

# insert event evnt into evnts in simlist
# library functions (do not alter)
insevnt <- function(evnt,simlist) {
  # if the event set is empty, set it to consist of evnt and return
  # cat("new arrival/completion evnt:")
  # print(evnt)
  # print("\n")
  if (is.null(simlist$evnts)) {
    simlist$evnts <- matrix(evnt,nrow=1)
    return()
  }
  # otherwise, find insertion point as inspt based on timeline
  # compare the occurance time
  # evnts only keep the boundary between arrival and completion event
  inspt <- binsearch(simlist$evnts[,1],evnt[1])
  # now "insert," by reconstructing the matrix; we find what portion of
  # the current matrix should come before evnt and what portion should 
  # come after it, then string everything together
  before <- if (inspt == 1) NULL else simlist$evnts[1:(inspt-1),]
  nr <- nrow(simlist$evnts)
  after <- if (inspt <= nr) simlist$evnts[inspt:nr,] else NULL  
  simlist$evnts <- rbind(before,evnt,after)  
  rownames(simlist$evnts) <- NULL
  # cat("event list:")
  # print(simlist$evnts[,1])
  # print(inspt)
  # cat("before:")
  # print(before)
  # cat("after:")
  # print(after)
}

# schedule new event in evnts in simlist; evnttime is the time at
# which the event is to occur; evnttype is the event type; appdata is
# a vector of numerical application-specific data (do not alter)
schedevnt <- function(evnttime,evnttype,simlist,appdata=NULL) {
  evnt <- c(evnttime,evnttype,appdata)
  insevnt(evnt,simlist)  
}

# start to process next event (second half done by application
# programmer via call to reactevnt() from mainloop()) (do not alter)
getnextevnt <- function(simlist) {
  head <- simlist$evnts[1,]
  # delete head
  if (nrow(simlist$evnts) == 1) simlist$evnts <- NULL else 
    simlist$evnts <- simlist$evnts[-1,,drop=F]  
  return(head)
}

# main loop of the simulation (do not alter)
mainloop <- function(simlist,simtimelim) {
  while(simlist$currtime < simtimelim) {
    head <- getnextevnt(simlist) # extract head event
    # update current simulated time
    simlist$currtime <- head[1]  
    # process this event (programmer-supplied ftn)
    simlist$reactevent(head,simlist)  
    if (simlist$dbg) {
      print("event occurred: current time; type: arrival or service completion; data: timeofnextarrival & job num;")
      print(head)
      print("events list now: before, after")
      print(simlist$evnts)
      # enter R browser for single-stepping
      print("queue: ")
      print(simlist$queue)
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

# what new events are triggered by the occurrence of an old one?
# M/M/S/∞ 表示输入过程是Poisson流, 服务时间服从负
# 指数分布,  系统有S个服务台平行服务, 系统容量为无穷的
# 等待制排队系统.
# 即顾客到达系统的相继到达时间间隔独立，且服从参数
# 为λ的负指数分布(即输入过程为Poisson过程), 服务台
# 的服务时间也独立同分布,  且服从参数为μ的负指数分
# 布，而且系统空间无限，允许永远排队.
# input the old event 
mm1react <- function(evnt,simlist) {  
  # print(simlist$lastTime + simlist$currtime)
  simlist$stateDuration[simlist$state + 1] <- simlist$stateDuration[simlist$state + 1] - simlist$lastTime + simlist$currtime
  simlist$lastTime = simlist$currtime
  etype <- evnt[2]
  if (etype == 1) {  # job arrival
    simlist$state = simlist$state + 1 # state indicator++
    # using the queue to buffer the broken machine/event
    simlist$queue <- appendtofcfsqueue(simlist$queue,evnt) # add to queue  
    srvduration <- rexp(1, simlist$state * simlist$srvrate)
    if (simlist$state == simlist$k) {
      # print("yell")
      tmp <- delfcfsqueuehead(simlist$queue)# check the arrival time for the first user in the queue
      job <- tmp$qhead
      # simlist$stateDuration[simlist$state + 1, 1] = simlist$stateDuration[simlist$state + 1, 1] + srvduration
      # simlist$lastTime <- simlist$currtime + srvduration
      # schedevnt(simlist$currtime+srvduration, 2, simlist, job[3:4])
      schedevnt(simlist$currtime + srvduration, 2, simlist, evnt[3:4])
    } else {
      # compare  timeofnextarrival and srvduration  
      timeofnextarrival <- rexp(1, (simlist$k - simlist$state) * simlist$arrvrate)      
      if (timeofnextarrival < srvduration) {
        jobnum <- simlist$jobnum + 1
        simlist$jobnum <- jobnum
        #simlist$stateDuration[simlist$state + 1, 1] = simlist$stateDuration[simlist$state + 1, 1] + timeofnextarrival 
        # simlist$lastTime <- simlist$currtime + timeofnextarrival
        # schedule next arrival
        # schedevnt(simlist$lastTime, 1, simlist, c(timeofnextarrival,jobnum))
        timeofnextarrival <- simlist$currtime + timeofnextarrival
        schedevnt(timeofnextarrival, 1, simlist, c(timeofnextarrival,jobnum))
      } else {
        tmp <- delfcfsqueuehead(simlist$queue)# check the arrival time for the first user in the queue
        job <- tmp$qhead
        #simlist$stateDuration[simlist$state + 1, 1] = simlist$stateDuration[simlist$state + 1, 1] + srvduration
        # simlist$lastTime <- simlist$currtime + srvduration
        # schedule a service/repairmen
        # schedevnt(simlist$lastTime, 2, simlist, job[3:4])
        # schedevnt(simlist$currtime + srvduration, 2, simlist, job[3:4])
        schedevnt(simlist$currtime + srvduration, 2, simlist, evnt[3:4])
      }
    }
  } else if (etype == 2) {  # job completion
    # bookkeeping   
    # extract and del the head
    simlist$state <- simlist$state - 1    
    tmp <- delfcfsqueuehead(simlist$queue)
    job <- tmp$qhead
    simlist$queue <- tmp$newqueue
    
    simlist$totjobs <- simlist$totjobs + 1
    simlist$totwait <- simlist$totwait + simlist$currtime - evnt[3]    
    timeofnextarrival <- rexp(1, (simlist$k - simlist$state) * simlist$arrvrate)
    # check queue for waiting jobs
    #if (!is.null(simlist$queue)) {
    if (simlist$state != 0) {
      # compare  timeofnextarrival and srvduration     
      srvduration <- rexp(1, simlist$state * simlist$srvrate)
      if (timeofnextarrival < srvduration) {
        jobnum <- simlist$jobnum + 1
        simlist$jobnum <- jobnum
        #simlist$stateDuration[simlist$state + 1, 1] = simlist$stateDuration[simlist$state + 1, 1] + timeofnextarrival
        # simlist$lastTime <- simlist$currtime + timeofnextarrival
        timeofnextarrival <- simlist$currtime + timeofnextarrival
        schedevnt(timeofnextarrival, 1, simlist, c(timeofnextarrival,jobnum))
      } else {  
        tmp <- delfcfsqueuehead(simlist$queue)# check the arrival time for the first user in the queue
        job <- tmp$qhead
        #simlist$stateDuration[simlist$state + 1, 1] = simlist$stateDuration[simlist$state + 1, 1] + srvduration
        # simlist$lastTime <- simlist$currtime +srvduration
        # start job service    
        # schedevnt(simlist$currtime+srvduration,2,simlist, job[3:4])
        schedevnt(simlist$currtime + srvduration, 2, simlist, job[3:4])
      }
    } else {
      # print("yell2")
      jobnum <- simlist$jobnum + 1
      simlist$jobnum <- jobnum
      #simlist$stateDuration[simlist$state + 1, 1] = simlist$stateDuration[simlist$state + 1, 1] + timeofnextarrival
      # simlist$lastTime <- simlist$currtime + timeofnextarrival
      # schedevnt(simlist$lastTime, 1, simlist, job[3:4])
      timeofnextarrival <- simlist$currtime + timeofnextarrival
      schedevnt(timeofnextarrival, 1, simlist, c(timeofnextarrival,jobnum))
    }
  } 
}

# test; M/M/1 queue--exponential ("Markov" job interarrivals,
# exponential service times, 1 server
# main function
# input total machine number, event inter arrive mean time (machine mean life time),
# repairer serves mean time, 
# running time limit for main loop
machineRepair1b <- function(k, u, r, timelim,dbg=F) {
  simlist <- newsim(dbg) # new simlist
  simlist$reactevent <- mm1react  # set called reactevent function in simlist
  # set application-specific variables in simlist
  simlist$k <- k
  simlist$arrvrate <- 1 / r # set arrive rate lamda, which is the reciprocol of the mean time
  simlist$srvrate <- 1 / u # set serving rate r
  simlist$stateDuration <- rep(0, k + 1) # duration of each state
  simlist$pi <- rep(0, k + 1)
  simlist$state <- 0 # state indicator fot current system
  simlist$lastTime <- 0
  
  simlist$totjobs <- 0 # total jobs number
  simlist$totwait <- 0.0 # total wait time
  simlist$queue <- NULL
  simlist$srvrbusy <- F
  # defining job numbers is good practice, always invaluable during
  # debugging
  simlist$jobnum <- 0
  # event type codes: 1 for arrival, 2 for service completion;
  # set up first event, including info on this job's arrival time for
  # PS: event == job
  # later use in finding mean wait until job done
  # generate a random variable (event/job's coming time&bulb's lifetime) with exponential distribution cdf as arrival time
  timeto1starrival <- rexp(1, simlist$k * simlist$arrvrate) 
  jobnum <- simlist$jobnum + 1 # set the job num 编号
  simlist$jobnum <- jobnum
  # schedule a new event at time timeto1starrival (occurance time), type "arrival", insert it into simlist,
  # jobnum and timeto1starrival as attached data
  schedevnt(timeto1starrival,1,simlist,c(timeto1starrival,jobnum))
  # simlist$stateDuration = simlist$evnts(1)  #state 0
  mainloop(simlist,timelim)
  # should print out 1 / (srvrate - arrvrate)
  cat("total job/event number:  ")
  print(simlist$jobnum)
  # cat("mean wait:  ")
  # print(simlist$totwait / simlist$totjobs)
  # what is the relationship between pi: propotion of time at that state
  #cat("State Duration: ")
  #print(simlist$stateDuration)
  # cat("current time: ")
  # print(simlist$currtime)
  simlist$pi = simlist$stateDuration/simlist$lastTime
  cat("Pi: ")
  print(simlist$pi)
  cat("w: ")
  print(sum((0:k) * simlist$pi))
}

machineRepair1b(10,25,8,100000)
