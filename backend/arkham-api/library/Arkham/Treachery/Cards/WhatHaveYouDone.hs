module Arkham.Treachery.Cards.WhatHaveYouDone (whatHaveYouDone, WhatHaveYouDone (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WhatHaveYouDone = WhatHaveYouDone TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatHaveYouDone :: TreacheryCard WhatHaveYouDone
whatHaveYouDone = treachery WhatHaveYouDone Cards.whatHaveYouDone

instance HasModifiersFor WhatHaveYouDone where
  getModifiersFor (WhatHaveYouDone attrs) = case attrs.placement of
    InThreatArea iid -> do
      abilities <- select (AbilityIsAction #parley)
      modifyEach attrs (map (AbilityTarget iid) abilities) [AdditionalCost DiscardRandomCardCost]
    _ -> pure mempty

instance HasAbilities WhatHaveYouDone where
  getAbilities (WhatHaveYouDone attrs) =
    [ restrictedAbility attrs 1 (OnSameLocation <> CanManipulateDeck) actionAbility
    ]

instance RunMessage WhatHaveYouDone where
  runMessage msg t@(WhatHaveYouDone attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      shuffleIntoDeck iid attrs
      pure t
    _ -> WhatHaveYouDone <$> liftRunMessage msg attrs
