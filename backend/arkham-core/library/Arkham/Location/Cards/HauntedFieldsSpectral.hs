module Arkham.Location.Cards.HauntedFieldsSpectral (
  hauntedFieldsSpectral,
  HauntedFieldsSpectral (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait (Trait (Spectral))

newtype HauntedFieldsSpectral = HauntedFieldsSpectral LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

hauntedFieldsSpectral :: LocationCard HauntedFieldsSpectral
hauntedFieldsSpectral = location HauntedFieldsSpectral Cards.hauntedFieldsSpectral 3 (Static 0)

instance HasModifiersFor HauntedFieldsSpectral where
  getModifiersFor (EnemyTarget eid) (HauntedFieldsSpectral attrs) = do
    affected <- eid <=~> (enemyAt (toId attrs) <> EnemyWithTrait Spectral)
    pure $ toModifiers attrs [HorrorDealt 1 | affected]
  getModifiersFor _ _ = pure []

instance HasAbilities HauntedFieldsSpectral where
  getAbilities (HauntedFieldsSpectral a) =
    withRevealedAbilities
      a
      [haunted "Move the nearest Spectral enemy once toward Haunted Fields." a 1]

instance RunMessage HauntedFieldsSpectral where
  runMessage msg l@(HauntedFieldsSpectral attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      enemies <- selectList $ NearestEnemy $ EnemyWithTrait Spectral
      unless (null enemies) $ do
        player <- getPlayer iid
        push
          $ chooseOne
            player
            [ targetLabel
              enemy
              [MoveToward (EnemyTarget enemy) (LocationWithId $ toId attrs)]
            | enemy <- enemies
            ]
      pure l
    Flip _ _ target | isTarget attrs target -> do
      regular <- genCard Locations.hauntedFields
      push $ ReplaceLocation (toId attrs) regular Swap
      pure l
    _ -> HauntedFieldsSpectral <$> runMessage msg attrs
