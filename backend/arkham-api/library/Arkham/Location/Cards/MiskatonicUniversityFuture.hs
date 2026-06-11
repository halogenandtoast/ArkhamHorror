module Arkham.Location.Cards.MiskatonicUniversityFuture (miskatonicUniversityFuture) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Scientist))

newtype MiskatonicUniversityFuture = MiskatonicUniversityFuture LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicUniversityFuture :: LocationCard MiskatonicUniversityFuture
miskatonicUniversityFuture =
  location MiskatonicUniversityFuture Cards.miskatonicUniversityFuture 3 (PerPlayer 1)

instance HasAbilities MiskatonicUniversityFuture where
  getAbilities (MiskatonicUniversityFuture a) =
    extendRevealed
      a
      [ groupLimit PerGame
          $ restricted a 1 (Here <> exists (AssetWithTitle "Mary Zielinski" <> AssetAt (be a))) actionAbility
      , groupLimit PerGame
          $ restricted a 2 (Here <> Remembered ATreeSeedHasBeenPlanted) (FastAbility Free)
      ]

instance RunMessage MiskatonicUniversityFuture where
  runMessage msg l@(MiskatonicUniversityFuture attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      pastUniversity <- selectOne $ locationIs Cards.miskatonicUniversityPast
      for_ pastUniversity \past -> placeTokens (attrs.ability 1) past Token.Seed 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      n <- selectCount $ AssetWithTrait Scientist <> AssetAt (be attrs)
      gainClues iid (attrs.ability 2) n
      pure l
    _ -> MiskatonicUniversityFuture <$> liftRunMessage msg attrs
