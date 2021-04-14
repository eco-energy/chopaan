{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards     #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DataKinds #-}

module Chopaan.Ui.AddKbtz where
{--
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
import Chopaan.Kibbutz.KbtzimT
import Chopaan.Kibbutz.KbtzId

import Shpadoinkle.Widgets.Types

import           Shpadoinkle                       (Html, MonadJSM, text)
import qualified Shpadoinkle.Html                  as H
import           Shpadoinkle.Lens
import           Shpadoinkle.Router                (navigate, toHydration)
import           Shpadoinkle.Run                   (Env, entrypoint)
import           Shpadoinkle.Widgets.Form.Dropdown as Dropdown (Dropdown (..),
                                                                Theme (..),
                                                                defConfig,
                                                                dropdown)
import qualified Shpadoinkle.Widgets.Form.Input    as Input
import           Shpadoinkle.Widgets.Table         as Table
import           Shpadoinkle.Widgets.Types         (Consideration, Considered,
                                                    ConsideredChoice,
                                                    Control (..), Field,
                                                    Hygiene (..), Input (..),
                                                    Pick (..), Present,
                                                    Selected, Status (..),
                                                    Toggle (..), Validated (..),
                                                    fullset, fuzzySearch,
                                                    getValid, humanize, present,
                                                    validate, withOptions')




type Loc = (Double, Double)

newtype User = User { unUser :: Text } deriving (Eq, Ord, Show, Generic, NFData)

data KbtzUpdate s = KbtzUpdate
  { _location :: Field s Loc Input Loc
  , _name :: Field s Text Input Text
  , _nodes :: Field s [Text] Input [Nodezim]
  } deriving (Generic)


toEditForm :: Kbtzim -> KbtzUpdate 'Edit
toEditForm k = KbtzUpdate undefined


formGroup :: [Html m a] -> Html m a
formGroup = H.div "form-group row"


textControl
  :: forall t m a
   . Eq t => IsString t => Coercible Text t => MonadJSM m
  => (forall v. Lens' (a v) (Field v Text Input (Maybe t)))
  -> Text -> a 'Errors -> a 'Edit -> Html m (a 'Edit)
textControl l msg errs ef = formGroup
  [ H.label [ H.for' hName, H.class' "col-sm-2 col-form-label" ] [ text msg ]
  , H.div "col-sm-10" $
    [ ef <% l . mapping (fromMaybe "" `iso` noEmpty) $ Input.text
      [ H.name' hName
      , H.class' ("form-control":controlClass (errs ^. l) (ef ^. l .hygiene))
      ]
    ]
    <> invalid (errs ^. l) (ef ^. l . hygiene)
  ] where hName = toHtmlName msg
          noEmpty "" = Nothing
          noEmpty x  = Just x


intControl
  :: forall n m a
   . MonadJSM m => Integral n => Show n
  => (forall v. Lens' (a v) (Field v Text Input n))
  -> Text -> a 'Errors -> a 'Edit -> Html m (a 'Edit)
intControl l msg errs ef = formGroup
  [ H.label [ H.for' hName, H.class' "col-sm-2 col-form-label" ] [ text msg ]
  , H.div "col-sm-10" $
    [ ef <% l $ Input.integral
      [ H.name' hName, H.step "1", H.min "0"
      , H.class' ("form-control":controlClass (errs ^. l) (ef ^. l .hygiene))
      ]
    ]
    <> invalid (errs ^. l) (ef ^. l . hygiene)
  ] where hName = toHtmlName msg


selectControl
  :: forall p x m a
   . MonadJSM m => Control (Dropdown p)
  => Considered p ~ Maybe => Consideration ConsideredChoice p
  => Present (Selected p x) => Ord x => Present x
  => (forall v. Lens' (a v) (Field v Text (Dropdown p) x))
  -> Text -> a 'Errors -> a 'Edit -> Html m (a 'Edit)
selectControl l msg errs ef = formGroup
  [ H.label [ H.for' (toHtmlName msg)
            , H.class' "col-sm-2 col-form-label" ] [ text msg ]
  , H.div "col-sm-10" $
    [ ef <% l $ dropdown bootstrap defConfig ]
    <> invalid (errs ^. l) (ef ^. l . hygiene)
  ]
  where
  bootstrap Dropdown {..} = Dropdown.Theme
    { _wrapper = H.div
      [ H.class' [ ("dropdown", True)
                 , ("show", _toggle == Open) ]
      ]
    , _header  = pure . H.button
      [ H.class' ([ "btn", "btn-secondary", "dropdown-toggle" ] :: [Text])
      , H.type' "button"
      ] . present
    , _list    = H.div
      [ H.class' [ ("dropdown-menu", True)
                 , ("show", _toggle == Open) ]
      ]
    , _item    = H.a [ H.className "dropdown-item"
                     , H.textProperty "style" "cursor:pointer" ] . present
    }


controlClass :: Validated e a -> Hygiene -> [Text]
controlClass (Invalid _ _) Dirty = ["is-invalid"]
controlClass (Validated _) Dirty = ["is-valid"]
controlClass _ Clean             = []


invalid :: Validated Text a -> Hygiene -> [ Html m b ]
invalid (Invalid err errs) Dirty = (\e -> H.div "invalid-feedback" [ text e ]) <$> err:errs
invalid _                  _     = []


toHtmlName :: Text -> Text
toHtmlName = toLower . replace " " "-"


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
