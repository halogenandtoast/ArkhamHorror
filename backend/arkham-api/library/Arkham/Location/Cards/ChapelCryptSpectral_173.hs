module Arkham.Location.Cards.ChapelCryptSpectral_173 (
  chapelCryptSpectral_173,
  ChapelCryptSpectral_173 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection

newtype ChapelCryptSpectral_173 = ChapelCryptSpectral_173 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelCryptSpectral_173 :: LocationCard ChapelCryptSpectral_173
chapelCryptSpectral_173 = location ChapelCryptSpectral_173 Cards.chapelCryptSpectral_173 6 (Static 0)

instance HasModifiersFor ChapelCryptSpectral_173 where
  getModifiersFor target (ChapelCryptSpectral_173 attrs) | isTarget attrs target = do
    shouldModifyShroud <- selectNone $ enemyAt (toId attrs) <> ReadyEnemy
    pure $ toModifiers attrs [ShroudModifier (-3) | shouldModifyShroud]
  getModifiersFor _ _ = pure []

instance HasAbilities ChapelCryptSpectral_173 where
  getAbilities (ChapelCryptSpectral_173 attrs) =
    withRevealedAbilities
      attrs
      [ haunted
          "Spawn the top card of your deck facedown, engaged with you. Treat that card as a Reanimated Dead enemy with 1 fight, 1 health, 1 evade, 1 damage, and the Monster trait."
          attrs
          1
      ]

instance RunMessage ChapelCryptSpectral_173 where
  runMessage msg l@(ChapelCryptSpectral_173 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      spectral <- genCard Locations.chapelCryptSpectral_173
      push $ ReplaceLocation (toId attrs) spectral Swap
      pure l
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      mTopCard <- fieldMap InvestigatorDeck (listToMaybe . take 1 . unDeck) iid
      for_ mTopCard $ \topCard -> do
        let reanimatedDead = PlayerCard $ topCard {pcCardCode = "xreanimated"}
        placement <- createEnemy reanimatedDead iid
        pushAll
          [ RemovePlayerCardFromGame False (PlayerCard topCard)
          , toMessage placement
          ]
      pure l
    _ -> ChapelCryptSpectral_173 <$> runMessage msg attrs
