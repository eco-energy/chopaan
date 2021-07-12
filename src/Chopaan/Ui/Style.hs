{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Chopaan.Ui.Style where

import Shpadoinkle.Html.TH.CSS

$(extractNamespace "./assets/tailwind.min.css")

$(extractNamespace "./assets/style.css")

