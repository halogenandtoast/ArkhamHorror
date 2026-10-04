module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Assets.LittleGemma (littleGemma) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyClues))
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Assets.Hybrid
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Trait (Trait (Suspect))

newtype LittleGemma = LittleGemma AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

littleGemma :: AssetCard LittleGemma
littleGemma = ally LittleGemma Cards.littleGemma (0, 1)

instance HasAbilities LittleGemma where
  getAbilities (LittleGemma a) =
    [ removeFromGameWhenDefeated a 1
    , controlled a 2 (exists $ suspectAtYourLocation <> EnemyWithAnyClues)
        $ FastAbility
        $ exhaust a
    ]

suspectAtYourLocation :: EnemyMatcher
suspectAtYourLocation = EnemyWithTrait Suspect <> EnemyAt YourLocation

instance RunMessage LittleGemma where
  runMessage msg a@(LittleGemma attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putHybridIntoPlay iid attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      removeFromGame attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      suspects <- select $ suspectAtYourLocation <> EnemyWithAnyClues
      chooseTargetM iid suspects \eid -> do
        moveTokens (attrs.ability 2) eid iid #clue 1
        remaining <- field EnemyClues eid
        -- "Then, if there are no clues on that enemy, add it to the victory display."
        when (remaining <= 1) $ addToVictory iid eid
      pure a
    _ -> LittleGemma <$> liftRunMessage msg attrs
