{-# LANGUAGE TemplateHaskell #-}

module Arkham.Act (
  Act (..),
  lookupAct,
) where

import Arkham.Prelude hiding (fold)

import Arkham.Act.Acts
import Arkham.Act.Attrs
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.Name
import Arkham.Query

$(buildEntity "Act")

instance HasAbilities Act where
  getAbilities = genericGetAbilities

instance
  ( ActRunner env
  , HasId LocationId env InvestigatorId
  , HasModifiersFor env ()
  ) =>
  RunMessage env Act
  where
  runMessage = genericRunMessage

instance Query LocationMatcher env => HasModifiersFor env Act where
  getModifiersFor = genericGetModifiersFor

instance HasStep ActStep env Act where
  getStep = getStep . toAttrs

instance HasCount ClueCount env Act where
  getCount = pure . ClueCount . fromMaybe 0 . actClues . toAttrs

instance Entity Act where
  type EntityId Act = ActId
  type EntityAttrs Act = ActAttrs

instance Named Act where
  toName = toName . toAttrs

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
