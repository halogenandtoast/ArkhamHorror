module Arkham.Treachery.Cards.WhatHaveYouDone (
  whatHaveYouDone,
  WhatHaveYouDone (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action (Action (Parley))
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WhatHaveYouDone = WhatHaveYouDone TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

whatHaveYouDone :: TreacheryCard WhatHaveYouDone
whatHaveYouDone = treachery WhatHaveYouDone Cards.whatHaveYouDone

instance HasModifiersFor WhatHaveYouDone where
  getModifiersFor (AbilityTarget iid ab) (WhatHaveYouDone attrs) | treacheryOnInvestigator iid attrs = do
    pure
      $ toModifiers attrs
      $ guard (Parley `elem` abilityActions ab)
      *> [AdditionalCost DiscardRandomCardCost]
  getModifiersFor _ _ = pure []

instance HasAbilities WhatHaveYouDone where
  getAbilities (WhatHaveYouDone attrs) =
    [ restrictedAbility attrs 1 (OnSameLocation <> CanManipulateDeck) actionAbility
    ]

instance RunMessage WhatHaveYouDone where
  runMessage msg t@(WhatHaveYouDone attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ ShuffleIntoDeck (Deck.InvestigatorDeck iid) (toTarget attrs)
      pure t
    _ -> WhatHaveYouDone <$> runMessage msg attrs
