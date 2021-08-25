{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards     #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DataKinds #-}

module Chopaan.Ui.AddKbtz where

import Data.Text as T
import Data.String
import           Control.Lens                      hiding (view)
import           Control.Lens.Unsound              (lensProduct)
import           Data.Coerce                       (Coercible)
import           Data.Maybe                        (fromMaybe, isNothing)

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import Chopaan.Node.NodeT
import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId

import Shpadoinkle.Widgets.Types

import           Shpadoinkle                       (Html, MonadJSM, text)
import qualified Shpadoinkle.Html                  as H
import           Shpadoinkle.Lens
import           Shpadoinkle.Router                (navigate, toHydration)





type Loc = (Double, Double)

newtype User = User { unUser :: Text } deriving (Eq, Ord, Show, Generic, NFData)

{--
data KbtzUpdate s = KbtzUpdate
  { _location :: Field s Loc Input Loc
  , _name :: Field s Text Input Text
  , _nodes :: Field s [Text] Input [Nodezim]
  } deriving (Generic)


toEditForm :: Kbtzim -> KbtzUpdate 'Edit
toEditForm k = KbtzUpdate undefined




editForm :: forall m. (CRUDSpaceCraft m, MonadJSM m) => Maybe SpaceCraftId -> SpaceCraftUpdate 'Edit -> Html m (SpaceCraftUpdate 'Edit)
editForm mid ef = H.div_

  [ intControl    @SKU                   sku         "SKU"           errs ef
  , textControl   @Description           description "Description"   errs ef
  , intControl    @SerialNumber          serial      "Serial Number" errs ef
  , selectControl @'One @Squadron        squadron    "Squadron"      errs ef
  , selectControl @'AtleastOne @Operable operable    "Operable"      errs ef
  , H.div "d-flex flex-row justify-content-end"

    [ H.button
      [ H.onClickM_ . navigate @(SPA m) $ RList mempty
      , H.class' "btn btn-secondary"
      ] [ "Cancel" ]

    , H.button
      [ H.onClickM_ $ case isValid of
         Nothing -> return ()
         Just up -> do
           case mid of Nothing  -> () <$ createSpaceCraft up
                       Just sid -> updateSpaceCraft sid up
           navigate @(SPA m) (RList mempty)
      , H.class' "btn btn-primary"
      , H.disabled $ isNothing isValid
      ] [ "Save" ]

    ]
  ] where errs = validate ef
          isValid = getValid errs


--}
