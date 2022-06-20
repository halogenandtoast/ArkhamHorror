{-# LANGUAGE TemplateHaskell #-}

module Arkham.Act (
  Act (..),
  lookupAct,
) where

import Arkham.Prelude hiding (fold)

import Arkham.Act.Acts
import Arkham.Act.Attrs
import Arkham.Card
import Arkham.Classes
import Arkham.Classes.Entity.TH
import Arkham.Id
import Data.Aeson.TH

$(buildEntity "Act")
$(deriveJSON defaultOptions ''Act)

instance HasAbilities Act where
  getAbilities = $(entityF "Act" 'getAbilities)

instance RunMessage Act where
  runMessage = $(entityRunMessage "Act")

instance HasModifiersFor Act where
  getModifiersFor = $(entityF2 "Act" 'getModifiersFor)

instance Entity Act where
  type EntityId Act = ActId
  type EntityAttrs Act = ActAttrs
  toId = toId . toAttrs
  toAttrs = $(entityF "Act" 'toAttrs)

instance TargetEntity Act where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Act where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

lookupAct :: ActId -> (Int -> Act)
lookupAct actId =
  fromJustNote ("Unknown act: " <> show actId) $ lookup actId allActs

allActs :: HashMap ActId (Int -> Act)
allActs =
  mapFromList $
    map
      (\cb -> (ActId (cbCardCode cb), \deckId -> cbCardBuilder cb (deckId, ActId (cbCardCode cb))))
      $(buildEntityLookupList "Act")
