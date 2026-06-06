module Arkham.Act.Cards.ExtraterrestrialPhysiology (extraterrestrialPhysiology) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyDamage))
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Helpers.SkillTest (withSkillTest, withSkillTestInvestigator)
import Arkham.Helpers.Window (getTotalDamage)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Modifier
import Arkham.Projection
import Arkham.Trait (Trait (Manifold, Oozified))

newtype ExtraterrestrialPhysiology = ExtraterrestrialPhysiology ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extraterrestrialPhysiology :: ActCard ExtraterrestrialPhysiology
extraterrestrialPhysiology = act (2, A) ExtraterrestrialPhysiology Cards.extraterrestrialPhysiology Nothing

instance HasAbilities ExtraterrestrialPhysiology where
  getAbilities (ExtraterrestrialPhysiology a) =
    [ mkAbility a 1
        $ forced
        $ EnemyTakeDamage #after AnyDamageEffect (enemyIs Enemies.vulnerableHeart) AnyValue AnySource
    , restricted a 2 (DuringSkillTest $ WhileAttackingAnEnemy $ enemyIs Enemies.vulnerableHeart)
        $ FastAbility
        $ ClueCost
        $ Static 1
    , mkAbility a 3 $ Objective $ freeReaction $ RoundEnds #when
    ]

manifoldHealth :: Card -> Int
manifoldHealth card
  | card `cardMatch` cardIs Enemies.oozeling = 3
  | card `cardMatch` cardIs Enemies.graspingOoze = 5
  | card `cardMatch` cardIs Enemies.cubicOoze = 4
  | card `cardMatch` cardIs Enemies.oozewraith = 7
  | otherwise = 0

discardUntilManifoldsHaveTotalHealthAtLeast :: Int -> [EncounterCard] -> [EncounterCard]
discardUntilManifoldsHaveTotalHealthAtLeast x = go 0 []
 where
  go _ discarded [] = discarded
  go n discarded _ | n >= x = discarded
  go n discarded (card : cards)
    | toCard card `cardMatch` CardWithTrait Manifold =
        go (n + manifoldHealth (toCard card)) (card : discarded) cards
    | otherwise = go n (card : discarded) cards

instance RunMessage ExtraterrestrialPhysiology where
  runMessage msg a@(ExtraterrestrialPhysiology attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getTotalDamage -> n) _ -> do
      subject8L08 <- selectJust $ enemyIs Enemies.subject8L08
      placeTokens (attrs.ability 1) subject8L08 #damage (2 * n)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> withSkillTestInvestigator \iid ->
        skillTestModifier sid (attrs.ability 2) iid (SkillModifier #combat 2)
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      heart <- selectJust $ enemyIs Enemies.vulnerableHeart
      x <- field EnemyDamage heart
      removeTokens (attrs.ability 3) (EnemyTarget heart) #damage x
      place heart (OutOfPlay SetAsideZone)
      when (x >= 1) do
        shuffleEncounterDiscardBackIn
        push $ DoStep x msg
      advanceActDeck attrs
      pure a
    DoStep x (AdvanceAct (isSide B attrs -> True) _ _) -> do
      lead <- getLead
      discarded <- discardUntilManifoldsHaveTotalHealthAtLeast x . unDeck <$> getEncounterDeck
      push $ DiscardTopOfEncounterDeck lead (length discarded) (attrs.ability 3) (Just $ toTarget attrs)
      pure a
    DiscardedTopOfEncounterDeck _ cards _ (isTarget attrs -> True) -> do
      enemies <- shuffle $ filterCards (CardWithTrait Manifold <> #enemy) cards
      push $ ForTargets (map (CardIdTarget . toCardId) enemies) $ ForTargets [] msg
      pure a
    ForTargets
      (CardIdTarget x : xs)
      (ForTargets invalids msg'@(DiscardedTopOfEncounterDeck _ _ _ (isTarget attrs -> True))) -> do
        let
          wrapper =
            case invalids of
              [] -> id
              _ -> (<> not_ (mapOneOf LocationWithId [lid | LocationTarget lid <- invalids]))

        locations <- select $ wrapper $ LocationWithTrait Oozified
        enemy <- fetchCard x
        leadChooseOneM do
          targets locations \loc -> do
            spawnEnemyAt_ enemy loc
            push $ ForTargets xs $ ForTargets (toTarget loc : invalids) msg'
        pure a
    _ -> ExtraterrestrialPhysiology <$> liftRunMessage msg attrs
