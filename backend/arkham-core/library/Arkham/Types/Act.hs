{-# LANGUAGE TemplateHaskell #-}
module Arkham.Types.Act
  ( Act(..)
  , lookupAct
  ) where

import Arkham.Prelude hiding (fold)

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Cards
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Decks
import Arkham.Types.Id
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Trait

$(buildEntity "Act")

deriving anyclass instance ActionRunner env => HasActions env Act

instance
  ( HasName env LocationId
  , ActRunner env
  , HasList UnderneathCard env ActDeck
  , HasList UnderneathCard env AgendaDeck
  , HasId LocationId env InvestigatorId
  , HasCount ResourceCount env LocationId
  , HasModifiersFor env ()
  )
  => RunMessage env Act where
  runMessage = genericRunMessage

instance HasSet Trait env LocationId => HasModifiersFor env Act where
  getModifiersFor = genericGetModifiersFor

instance HasStep Act ActStep where
  getStep = ask >>= runReaderT getStep . toAttrs

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

lookupAct :: ActId -> Act
lookupAct actId =
  fromJustNote ("Unknown act: " <> show actId) $ lookup actId allActs

allActs :: Map ActId Act
allActs = mapFromList $ map
  (\cb -> (ActId (cbCardCode cb), cbCardBuilder cb (ActId (cbCardCode cb))))
  $(buildEntityLookupList "Act")
