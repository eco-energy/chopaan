
let BatteryType : Type = < LAFlooded | LASealed | LIon >
let Resolution : Type =  < Second | Ten | Ten2 | Ten3 | Ten4 | Ten5 | Ten6 | Ten7 >

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
           , port = +5432
           , database = "chopaan"
           , user = "chopaan"
           , password = "testPassword" --"3423dssgSSS$%@!!01G"
           }
, hydrationOpts = { start = { day = +1
                            , month = +9
                            , year = +2021
                            }
                   , end = { day = +15
                           , month = +9
                           , year = +2021
                           }
                   , s3BucketName = "dosti-datastream"
                   , dbSave = False
                   , resolution = Resolution.Ten3
                   , bufOpts = { prefixBuffer = +10
                               , pathBuffer = +100
                               , frameBuffer = +100
                               , nodeBuffer = +10
                               }
                   , hPrefix = "test2"
                   }
, poolConf = { pNumStripes = +5
             , reaperWait =  5.0
             , maxConnsPerStripe = Natural/toInteger 25
             }
}
