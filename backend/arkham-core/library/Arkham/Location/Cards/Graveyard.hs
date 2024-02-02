module Arkham.Location.Cards.Graveyard where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (graveyard)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Timing qualified as Timing

newtype Graveyard = Graveyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

graveyard :: LocationCard Graveyard
graveyard = location Graveyard Cards.graveyard 1 (PerPlayer 2)

instance HasAbilities Graveyard where
  getAbilities (Graveyard x) =
    withRevealedAbilities x
      $ [ mkAbility x 1
            $ ForcedAbility
            $ Enters Timing.After Anyone (LocationWithId $ toId x)
        ]

instance RunMessage Graveyard where
  runMessage msg l@(Graveyard attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid (toAbilitySource attrs 1) iid #willpower 3
      pure l
    FailedSkillTest iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ -> do
      rivertown <- getJustLocationByName "Rivertown"
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [ Label "Take 2 horror" [assignHorror iid (toAbilitySource attrs 1) 2]
          , Label "Move to Rivertown" [MoveTo $ move (toAbilitySource attrs 1) iid rivertown]
          ]
      pure l
    _ -> Graveyard <$> runMessage msg attrs
