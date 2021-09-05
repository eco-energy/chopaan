
let BatteryType : Type = < LAFlooded | LASealed | LIon >
let Resolution : Type =  < Year | Month | Week | Day | Hour | Minute | Second >

in

{ logVerbose = True

, mqttOpts = { mqttURI = "mqtts://a1e7lyi19kctcn-ats.iot.ap-southeast-1.amazonaws.com"
             , certPath = "certs/chopaan.cert.pem"
             , keyPath = "certs/chopaan.private.key.pem"
             , caPath = "certs/ca.cert.pem"
             }
, nodeOpts = [ 
           { macAddress = "abcdefghi"
           , battery = [
             { _type = BatteryType.LAFlooded
             , cutOffVoltage = 11.5
             , maxV = 13.0
             , ampHours = 150.0
             }]
           , pv = [{
               vOC = 10.0
             , vMPP = 10.0
             , iMPP = 19.0
             , power = 250.0
             }]
           }
  ]
, kibbutzOpts = { name = "kibbutz-pilot-node" }
, dbOpts = { host = "timescale"
           , port = Natural/toInteger 5432
           , database = "chopaan"
           , user = "chopaan"
           , password = "testPassword" --"3423dssgSSS$%@!!01G"
           }
, hydrationOpts = { start = { day = +24
                            , month = +8
                            , year = +2021
                            }
                   , end = { day = +1
                           , month = +9
                           , year = +2021
                           }
                   , s3BucketName = "dosti-datastream"
                   , dbSave = True
                   , resolution = Resolution.Minute
                   , bufOpts = { prefixBuffer = +100
                               , pathBuffer = +100
                               , frameBuffer = +1500
                               , nodeBuffer = +0
                               }
                   , hPrefix = "deploy"
                   }
, poolConf = { pNumStripes = +10
             , reaperWait =  10.0
             , maxConnsPerStripe = Natural/toInteger 15
             }
}
