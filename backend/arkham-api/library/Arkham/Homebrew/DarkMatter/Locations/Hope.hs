module Arkham.Homebrew.DarkMatter.Locations.Hope (hope) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Hope = Hope LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hope :: LocationCard Hope
hope =
  symbolLabel $ location Hope Cards.hope 2 (Static 1)

-- | "[reaction] At the end of your turn: Heal 1 horror." / "[action]: Resign."
instance HasAbilities Hope where
  getAbilities (Hope a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> youExist InvestigatorWithAnyHorror) $ freeReaction $ TurnEnds #when You
      , locationResignAction a
      ]

instance RunMessage Hope where
  runMessage msg l@(Hope attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healHorror iid (attrs.ability 1) 1
      pure l
    _ -> Hope <$> liftRunMessage msg attrs
