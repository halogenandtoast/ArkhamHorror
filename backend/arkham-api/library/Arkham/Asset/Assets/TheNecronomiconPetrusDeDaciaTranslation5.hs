module Arkham.Asset.Assets.TheNecronomiconPetrusDeDaciaTranslation5 (
  theNecronomiconPetrusDeDaciaTranslation5,
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype TheNecronomiconPetrusDeDaciaTranslation5 = TheNecronomiconPetrusDeDaciaTranslation5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNecronomiconPetrusDeDaciaTranslation5 :: AssetCard TheNecronomiconPetrusDeDaciaTranslation5
theNecronomiconPetrusDeDaciaTranslation5 =
  asset TheNecronomiconPetrusDeDaciaTranslation5 Cards.theNecronomiconPetrusDeDaciaTranslation5

instance HasAbilities TheNecronomiconPetrusDeDaciaTranslation5 where
  getAbilities (TheNecronomiconPetrusDeDaciaTranslation5 a) =
    [ withTooltip "{fast} Spend 1 secret: You get +2 skill value for this skill test."
        $ controlled a 1 (DuringSkillTest AnySkillTest)
        $ FastAbility (assetUseCost a #secret 1)
    , withTooltip "{fast} Spend 2 secrets: Draw 2 cards."
        $ controlled a 2 CanDrawCards
        $ FastAbility (assetUseCost a #secret 2)
    , withTooltip "{fast} Spend 3 secrets: Discover 1 clue at any location."
        $ controlled a 3 (CanDiscoverCluesAt Anywhere)
        $ FastAbility (assetUseCost a #secret 3)
    , withTooltip "{fast} Spend 4 secrets: Deal 3 damage to an enemy engaged with you."
        $ withCriteria (mkAbility a 4 $ FastAbility $ assetUseCost a #secret 4)
        $ ControlsThis
        <> exists (EnemyIsEngagedWith You <> EnemyCanBeDamagedBySource (toSource a))
    ]

instance RunMessage TheNecronomiconPetrusDeDaciaTranslation5 where
  runMessage msg a@(TheNecronomiconPetrusDeDaciaTranslation5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      drawCards iid (attrs.ability 2) 2
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      lids <- select $ LocationWithDiscoverableCluesBy $ InvestigatorWithId iid
      chooseTargetM iid lids \lid -> discoverAt NotInvestigate iid (attrs.ability 3) lid 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 4 -> do
      eids <- select $ enemyEngagedWith iid <> EnemyCanBeDamagedBySource (toSource attrs)
      chooseTargetM iid eids $ nonAttackEnemyDamage (Just iid) (attrs.ability 4) 3
      pure a
    _ -> TheNecronomiconPetrusDeDaciaTranslation5 <$> liftRunMessage msg attrs
