
let BatteryType : Type = < LAFlooded | LASealed | LIon >

let TxStart : Type = < Immediately | WithDelay Int >

in { logVerbose = True
, mqttOpts = { connId = "chopaan-pilot-1"
             , mqttURI = "mqtts://a1e7lyi19kctcn-ats.iot.ap-southeast-1.amazonaws.com"
             , certPath = "certs/chopaan.cert.pem"
             , keyPath = "certs/chopaan.private.key.pem"
             , caPath = "certs/ca.cert.pem"
             }
, nodeOpts = [
           { macAddress = "abcdefghi"
           , battery = [{
               _type = BatteryType.LAFlooded
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
, transactions = { sender = "abcdefghi"
                 , reciever = "abcdefghi"
                 , power = "100"
                 , duration = "60"
                 , repeatFor = 20
                 , start = WithDelay 20
                 }
}