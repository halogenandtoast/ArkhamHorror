module Arkham.Treachery.Cards.WhatHaveYouDone (whatHaveYouDone, WhatHaveYouDone (..)) where

import Arkham.Ability
import Arkham.Action (Action (Parley))
import Arkham.Helpers.Modifiers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WhatHaveYouDone = WhatHaveYouDone TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatHaveYouDone :: TreacheryCard WhatHaveYouDone
whatHaveYouDone = treachery WhatHaveYouDone Cards.whatHaveYouDone

instance HasModifiersFor WhatHaveYouDone where
  getModifiersFor (AbilityTarget iid ab) (WhatHaveYouDone attrs) | treacheryInThreatArea iid attrs = do
    modified attrs
      $ guard (Parley `elem` abilityActions ab)
      *> [AdditionalCost DiscardRandomCardCost]
  getModifiersFor _ _ = pure []

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
