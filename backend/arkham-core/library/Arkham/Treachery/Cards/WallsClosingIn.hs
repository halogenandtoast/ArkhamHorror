module Arkham.Treachery.Cards.WallsClosingIn (
  wallsClosingIn,
  WallsClosingIn (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WallsClosingIn = WallsClosingIn TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wallsClosingIn :: TreacheryCard WallsClosingIn
wallsClosingIn = treachery WallsClosingIn Cards.wallsClosingIn

instance RunMessage WallsClosingIn where
  runMessage msg t@(WallsClosingIn attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      shroud <- maybe (pure 0) (field LocationShroud) =<< field InvestigatorLocation iid
      t <$ push (RevelationSkillTest iid source SkillWillpower shroud)
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ n
      | isSource attrs source -> do
          push
            $ chooseOne
              iid
              [ Label
                  ("Take " <> tshow n <> " horror")
                  [InvestigatorAssignDamage iid source DamageAny 0 n]
              , Label
                  "Randomly choose 1 enemy from among the set-aside Monster enemies and place it beneath the act deck without looking at it"
                  [DrawRandomFromScenarioDeck iid MonstersDeck (toTarget attrs) 1]
              ]
          pure t
    DrewFromScenarioDeck _ _ target cards
      | isTarget attrs target ->
          t <$ push (PlaceUnderneath ActDeckTarget cards)
    _ -> WallsClosingIn <$> runMessage msg attrs
