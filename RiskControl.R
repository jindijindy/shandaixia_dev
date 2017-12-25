test1 <- c("st60065");test2 <- c("st60066")
white_list <- c("st30010")
have_borrowed <- c("st30012")
###tongdun reject
modelTwoRejectSet <- c("st10030","st10029")
baiqishiRejectSet <- c("st10029")
baiqishiWhiteReject <- c("st10071","st10072","st10073","st10074","st10075",
                         "st10076","st10077","st10078","st10079","st10080","st10082",
                         "st10083","st10084","st10085","st10086","st10087","st10088",
                         "st10089","st10090","st10091","st10092","st10093","st10094",
                         "st10095","st10096","st10097","st10098","st10099","st10100",
                         "st10101","st10102","st10103","st10104","st10105")
tongdunRejectSet <- c("st10063","st10064","st10065","st10066","st10201","st10205","st10206","st10207","st10208","st10216")
###for white_list
wlAccessSet <- c("st40001","st40002","st40003","st40004","st40005","st30008")
###for normal_list
normalListAccessSet1 <- c("st50010","st50011","st50014","st50015","st50016")
normalListAccessSet2 <- c("st50010","st50011","st50013","st50015","st50016")
###
haveBorrowedAccessSet <- c("st50010","st50013")
###direct access
oldClientAccessSet <- c("st30004","st30011")
oldClientReviewSet <- c("st10046","st20035")
oldClientWhiteReviewSet <- c("st10046","st20035")
oldClientRejectSet <- c("st10027","st10011","st10046","st10062","st10037","st10901")
oldClientWhiteRejectSet <- c("st10011","st10027","st10062","st10046","st10901")
###
newClientrejectSet <- c("st10002","st10003","st10004","st10005",
               "st10006","st10007","st10008","st10009","st10010",
               "st10011","st10012","st10013","st10014","st10015",
               "st10016","st10017","st10018","st10019","st10020",
               "st10021","st10022","st10023","st10025",
               "st10026","st10027","st10028",
               "st10031","st10032","st10033","st10034","st10035",
               "st10036","st10037","st10038","st10039","st10040", 
               "st10041","st10042","st10043","st10044","st10045",
               "st10046","st10047","st10048","st10049",
               "st10051","st10052","st10053","st10054","st10055",
               "st10056","st10057","st10058","st10060",
               "st10061","st10062","st10063","st10064","st10065",
               "st10066","st10067","st10068","st10070",
               "st11001","st11002","st11003","st11004","st10302","st10301","st10305","st10106","st10303","st10304","st10501","st10502")
whiteClientrejectSet <- c("st10011","st10013","st10027","st10062","st10037","st10046",
                          "st10063","st10064","st10065","st10066","st10043","st10042","st10012","st10060","st10057","st10052","st10301")
haveBorrowedRejectSet <- c("st10011","st10013","st10027","st10062","st10037","st10046","st10051","st10049",
                          "st10043","st10042","st10301","st10003","st10004","st10302","st10303","st10304","st10025",
                          "st10060","st10015","st10056","st10057")
#* @post /RiskControl           
RiskControl <- function(JSON_Inputted){
  callTools<<-"RiskControl"
  ###git
  # source("/app/R/shandaixia/strategySet/Pretest_for_RiskControl.R", encoding = 'UTF-8')
  # source("/app/R/shandaixia/strategySet/Stratepy_tianjiReject.R", encoding = 'UTF-8')
  source("d:/git_workspace/FraudDetection/shandaixia_dev/strategySet/Pretest_for_RiskControl.R", encoding = 'UTF-8')
  source("d:/git_workspace/FraudDetection/shandaixia_dev/strategySet/Stratepy_tianjiReject.R", encoding = 'UTF-8')
  cat("load dunFile is OK!\n")
  test_res1 <- pretest_for_RiskControl(JSON_Inputted)
  if (test_res1$result){
    #Import the log4r package.
    library('log4r')
    # Create a new logger object with create.logger().
    logger <<- create.logger()
    dateDir <- as.character(Sys.Date())
    if(!dir.exists(paste0(test_res1$logPath,dateDir))){
      dir.create(paste0(test_res1$logPath,dateDir))
    }
    # Set the logger's file output.
    logfile(logger) <- paste0(test_res1$logPath,dateDir,"/RiskControlDebugging_",test_res1$userId,".log")
    # Set the current level of the logger.
    level(logger) <- 'INFO'
    info(logger, 'Risk Control Start')
    info(logger, paste("order_id:",test_res1$orderId))
    # if("userChannel"%in%names(test_res1)){}
    if(test_res1$stage == "init"){
      info(logger, "stage init")
      directRejectResultSet <- tryCatch(stratepy_directReject(user_id = test_res1$userId,order_id = test_res1$orderId,logger = logger),
                              error = function(e) {info(logger, paste("error info of aixin:",e,traceback(),sep=' '));
                                print(traceback());return(FALSE)})
      if("modelScore"%in%names(directRejectResultSet)){
        modelScore <- directRejectResultSet$modelScore
      }else{
        modelScore <- 607.56
      }
      ###try_Catch_final
      if (!is.logical(directRejectResultSet)){
        if (!is.null(directRejectResultSet$errCode)){
          return(toJSON(list(errCode = directRejectResultSet$errCode,result=c(),modelScore = modelScore)))
        }else if (length(directRejectResultSet$result) == 0){
          cat("No hit ...OK!\n")
          return(toJSON(list(result = "baiqishi",errCode =NULL,strategySet = c(),userType = directRejectResultSet$user_type,modelScore = modelScore)))
        }else if("st60043"%in%directRejectResultSet$result){
          return(toJSON(list(result = "review",errCode =NULL,strategySet = directRejectResultSet$result,userType = directRejectResultSet$user_type,modelScore = modelScore)))
        }else if("st60065"%in%directRejectResultSet$result){
          return(toJSON(list(result = "baiqishi",errCode =NULL,strategySet = directRejectResultSet$result,userType = directRejectResultSet$user_type,modelScore = modelScore)))
        }else if("st60066"%in%directRejectResultSet$result){
          return(toJSON(list(result = "review",errCode =NULL,strategySet = directRejectResultSet$result,userType = directRejectResultSet$user_type,modelScore = modelScore)))
        }else if("st20044"%in%directRejectResultSet$result){
          return(toJSON(list(result = "reject",errCode =NULL,strategySet = directRejectResultSet$result,userType = directRejectResultSet$user_type,modelScore = modelScore)))
        }
        user_type = directRejectResultSet$user_type
        if(user_type == "newClient"&any(directRejectResultSet$result%in%have_borrowed)){
          if(!"st20046"%in%directRejectResultSet$result){
            user_type <- "newClient"
          }
        }else if(user_type == "newClient"&any(directRejectResultSet$result%in%white_list)&("st60044"%in%directRejectResultSet$result)){
          user_type <- "newClient"
        }
        mark_set <- c()
        if(user_type == "oldClient"){
          if(any(directRejectResultSet$result%in%oldClientRejectSet)){
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = directRejectResultSet$result,
                               userType = user_type,modelScore = modelScore)))
          }else if(any(directRejectResultSet$result%in%oldClientReviewSet)){
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = directRejectResultSet$result,
                               userType = user_type,modelScore = modelScore)))
          }else if(any(directRejectResultSet$result%in%oldClientAccessSet)){
            if("st20044"%in%directRejectResultSet$result){
              return(toJSON(list(result = "reject",errCode =NULL,strategySet = directRejectResultSet$result,
                                 userType = user_type,modelScore = modelScore)))
            }else if("st20099"%in%directRejectResultSet$result){
              return(toJSON(list(result = "review",errCode =NULL,strategySet = directRejectResultSet$result,
                                 userType = user_type,modelScore = modelScore)))
            }else{
              return(toJSON(list(result = "review",errCode =NULL,strategySet = directRejectResultSet$result,
                                 userType = user_type,modelScore = modelScore)))
            }
          }else{
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = directRejectResultSet$result,
                               userType = user_type,modelScore = modelScore)))
          }
        }else if(user_type == "newClientHaveBorrowed"){
          if(any(directRejectResultSet$result%in%haveBorrowedRejectSet)){
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = directRejectResultSet$result,
                               userType = user_type,modelScore = modelScore)))
          }else if(all(haveBorrowedAccessSet%in%directRejectResultSet$result)){
            if("st20044"%in%directRejectResultSet$result){
              return(toJSON(list(result = "reject",errCode =NULL,strategySet = directRejectResultSet$result,
                                 userType = user_type,modelScore = modelScore)))
            }else{
              return(toJSON(list(result = "reject",errCode =NULL,strategySet = directRejectResultSet$result,
                                 userType = user_type,modelScore = modelScore)))
            }
          }else{
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = directRejectResultSet$result,
                               userType = user_type,modelScore = modelScore)))
          }
        }else if(user_type == "newClient"){
          if(any(directRejectResultSet$result%in%newClientrejectSet)){
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = directRejectResultSet$result,
                               userType = user_type,modelScore = modelScore)))
          }else{
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = directRejectResultSet$result,
                               userType = user_type,modelScore = modelScore)))
          }
        }else if(user_type == "newClientWhite"){
          if(any(directRejectResultSet$result%in%whiteClientrejectSet)){
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = directRejectResultSet$result,
                               userType = user_type,modelScore = modelScore)))
          }else{
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = directRejectResultSet$result,
                               userType = user_type,modelScore = modelScore)))
          }
        }else{
          ##user_type wrong!
          return(toJSON(list(result = "T2345",errCode ="T2345")))
        }
      }else{
        return(toJSON(list(result = directRejectResultSet,errCode ="T1007")))
      }
    }else if(test_res1$stage == "baiqishi"){
      info(logger, "stage baiqishi")
      source("d:/git_workspace/FraudDetection/shandaixia_dev/strategySet/Stratepy_modelTwo.R", encoding = 'UTF-8')
      modelTwoResultSet <- tryCatch(stratepy_modelTwo(user_id = test_res1$userId,order_id = test_res1$orderId,logger = logger,stage= "baiqishi"),
                                    error = function(e) {info(logger, paste("error info of aixin_baiqishi:",e,traceback(),sep=' '));
                                      print(traceback());return(FALSE)})
      ##finalResult
      if (is.logical(modelTwoResultSet)){
        return(toJSON(list(result = modelTwoResultSet,errCode ="T1007")))
      }
      if (!is.null(modelTwoResultSet$errCode)){
        return(toJSON(list(errCode = modelTwoResultSet$errCode,result=c())))
      }else if (length(modelTwoResultSet$result) == 0){
        cat("No hit ...baiqishi wrong!\n")
        info(logger, "No hit ...baiqishi wrong!\n")
        return(toJSON(list(result = "review",errCode =NULL,strategySet = test_res1$strategySet,userType = test_res1$userType)))
      }else {
        user_type <- test_res1$userType
        mark_set <- c(test_res1$strategySet,modelTwoResultSet$result)
        if("st60065"%in%mark_set){
          return(toJSON(list(result = "tongdun",errCode =NULL,strategySet = mark_set,userType = directRejectResultSet$user_type)))
        }
        if(user_type == "oldClient"){
          if(any(modelTwoResultSet$result%in%baiqishiRejectSet)){
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }else{
            return(toJSON(list(result = "tongdun",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }
        }else if(user_type == "newClient"){
          if(any(modelTwoResultSet$result%in%baiqishiRejectSet)){
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }else{
            # mark_set <- c(mark_set,modelTwoResultSet$result)
            return(toJSON(list(result = "tongdun",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }
        }else if(user_type == "newClientWhite"){
          if(any(modelTwoResultSet$result%in%baiqishiWhiteReject)){
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }else{
            return(toJSON(list(result = "tongdun",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }
        }else if(user_type == "newClientHaveBorrowed"){
          if(any(modelTwoResultSet$result%in%baiqishiWhiteReject)){
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }else{
            return(toJSON(list(result = "tongdun",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }
          # return(toJSON(list(result = "review",errCode =NULL,strategySet = mark_set,userType = user_type)))
        }else{
          ##user_type wrong!
          return(toJSON(list(result = "T2345",errCode ="T2345")))
        }
      }
      
    }else if(test_res1$stage == "tongdun"){
      info(logger, "stage tongdun")
      source("d:/git_workspace/FraudDetection/shandaixia_dev/strategySet/Stratepy_modelTwo.R", encoding = 'UTF-8')
      modelTwoResultSet <- tryCatch(stratepy_modelTwo(user_id = test_res1$userId,order_id = test_res1$orderId,logger = logger,stage= "tongdun"),
                                    error = function(e) {info(logger, paste("error info of aixin_tongdun:",e,traceback(),sep=' '));
                                      print(traceback());return(FALSE)})
      ##finalResult
      if (is.logical(modelTwoResultSet)){
        return(toJSON(list(result = modelTwoResultSet,errCode ="T1007")))
      }
      if (!is.null(modelTwoResultSet$errCode)){
        return(toJSON(list(errCode = modelTwoResultSet$errCode,result=c())))
      }else {
        user_type = test_res1$userType
        mark_set <- c(test_res1$strategySet,modelTwoResultSet$result)
        if("st60065"%in%mark_set){
          return(toJSON(list(result = "review",errCode =NULL,strategySet = mark_set,userType = directRejectResultSet$user_type)))
        }
        if(user_type == "oldClient"){
          if(any(modelTwoResultSet$result%in%tongdunRejectSet)){
            return(toJSON(list(result = "review",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }else{
            return(toJSON(list(result = "review",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }
        }else if(user_type == "newClient"){
          if(any(modelTwoResultSet$result%in%tongdunRejectSet)){
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }else if("st10050"%in%mark_set){
            return(toJSON(list(result = "review",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }else if(all(normalListAccessSet1%in%mark_set)|all(normalListAccessSet2%in%mark_set)){
            if("st20044"%in%mark_set){
              return(toJSON(list(result = "review",errCode =NULL,strategySet = mark_set,userType = user_type)))
            }else{
              return(toJSON(list(result = "review",errCode =NULL,strategySet = mark_set,userType = user_type)))
            }
          }else if("st10307"%in%mark_set){
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }else{
            return(toJSON(list(result = "review",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }
        }else if(user_type == "newClientWhite"){
          if(any(modelTwoResultSet$result%in%tongdunRejectSet)){
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }else if(all(wlAccessSet%in%mark_set)){
            # return(toJSON(list(result = "access",errCode =NULL,strategySet = mark_set,userType = user_type)))
            return(toJSON(list(result = "review",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }else{
            return(toJSON(list(result = "review",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }
        }else if(user_type == "newClientHaveBorrowed"){
          if(any(modelTwoResultSet$result%in%tongdunRejectSet)){
            return(toJSON(list(result = "reject",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }else{
            return(toJSON(list(result = "review",errCode =NULL,strategySet = mark_set,userType = user_type)))
          }
        }else{
          ##user_type wrong!
          return(toJSON(list(result = "T2345",errCode ="T2345")))
        }
      }
      
    }else{
      ###NO stage code
      return(toJSON(list(result = "review",errCode ="T1234",strategySet = NULL)))
    }
  }else{
    #Import the log4r package.
    library('log4r')
    # Create a new logger object with create.logger().
    logger <- create.logger()
    # Set the logger's file output.
    logfile(logger) <- paste0("RiskControlDebugging_", format(Sys.time(), "%Y%m%d%H%M%S"),".log")
    # Set the current level of the logger.
    level(logger) <- 'INFO'
    info(logger, 'Risk Control Start')
    return(toJSON(list(result = test_res1$result,errCode =test_res1$errCode,userId = test_res1$user_id)))
  }
}
