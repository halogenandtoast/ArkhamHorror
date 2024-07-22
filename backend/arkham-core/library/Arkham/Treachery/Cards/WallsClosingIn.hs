module Arkham.Treachery.Cards.WallsClosingIn (wallsClosingIn, WallsClosingIn (..)) where

import Arkham.Choose
import Arkham.Classes
import Arkham.Location.Types (Field (..))
import Arkham.Prelude
import Arkham.Scenario.Deck
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WallsClosingIn = WallsClosingIn TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wallsClosingIn :: TreacheryCard WallsClosingIn
wallsClosingIn = treachery WallsClosingIn Cards.wallsClosingIn

instance RunMessage WallsClosingIn where
  runMessage msg t@(WallsClosingIn attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push
        $ revelationSkillTest sid iid attrs #willpower
        $ InvestigatorLocationMaybeFieldCalculation iid LocationShroud
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label ("Take " <> tshow n <> " horror") [assignHorror iid attrs n]
          , Label
              "Randomly choose 1 enemy from among the set-aside Monster enemies and place it beneath the act deck without looking at it"
              [ChooseFrom iid $ chooseRandom attrs MonstersDeck 1]
          ]
      pure t
    ChoseCards _ chosen | isTarget attrs chosen.target -> do
      push $ PlaceUnderneath ActDeckTarget chosen.cards
      pure t
    _ -> WallsClosingIn <$> runMessage msg attrs
