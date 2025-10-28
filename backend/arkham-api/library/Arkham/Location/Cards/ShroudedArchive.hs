module Arkham.Location.Cards.ShroudedArchive (shroudedArchive) where

import Arkham.Ability
import Arkham.Helpers.SkillTest.Target
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ShroudedArchive = ShroudedArchive LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shroudedArchive :: LocationCard ShroudedArchive
shroudedArchive = location ShroudedArchive Cards.shroudedArchive 4 (Static 0)

instance HasAbilities ShroudedArchive where
  getAbilities (ShroudedArchive a) = extendRevealed a [restricted a 1 (Here <> KeyCount (atLeast 2) KeyOnCard) actionAbility]

instance RunMessage ShroudedArchive where
  runMessage msg l@(ShroudedArchive attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ks <- select KeyOnCard
      chooseOneM iid $ for ks \k -> keyLabeled k.key $ forTarget (KeyTarget k.key) msg
      pure l
    ForTarget (KeyTarget k) (UseThisAbility iid (isSource attrs -> True) 1) -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) (KeyTarget k) [#willpower, #intellect]
        $ MaxCalculation (Fixed 0) (SubtractCalculation (Fixed 8) (CountEnemies #cultist))
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTarget >>= \case
        Just (KeyTarget kt) -> do
          k <- selectJust $ KeyIs kt
          ks <- filter ((/= k.key) . (.key)) <$> select AnyKey
          let
            swapKeys a b = swapKey a b >> swapKey b a
            swapKey a b =
              fromMaybe (error "must be key target")
                $ asum
                  [ (`placeKey` b.key) <$> a.keyLocation
                  , (`placeKey` b.key) <$> a.keyInvestigator
                  , (`placeKey` b.key) <$> a.keyAsset
                  , (`placeKey` b.key) <$> a.keyEnemy
                  , guard a.keySetAside $> placeKey GameTarget b.key
                  ]
          chooseOneM iid $ for ks \k' -> keyLabeled k'.key $ swapKeys k k'
        _ -> error "must be key target"
      pure l
    _ -> ShroudedArchive <$> liftRunMessage msg attrs
