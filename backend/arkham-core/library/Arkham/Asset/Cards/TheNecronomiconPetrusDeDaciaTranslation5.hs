module Arkham.Asset.Cards.TheNecronomiconPetrusDeDaciaTranslation5 (
  theNecronomiconPetrusDeDaciaTranslation5,
  TheNecronomiconPetrusDeDaciaTranslation5 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype TheNecronomiconPetrusDeDaciaTranslation5 = TheNecronomiconPetrusDeDaciaTranslation5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNecronomiconPetrusDeDaciaTranslation5
  :: AssetCard TheNecronomiconPetrusDeDaciaTranslation5
theNecronomiconPetrusDeDaciaTranslation5 =
  asset
    TheNecronomiconPetrusDeDaciaTranslation5
    Cards.theNecronomiconPetrusDeDaciaTranslation5

instance HasAbilities TheNecronomiconPetrusDeDaciaTranslation5 where
  getAbilities (TheNecronomiconPetrusDeDaciaTranslation5 a) =
    [ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility (assetUseCost a Secret 1)
    , restrictedAbility a 2 (ControlsThis <> CanDrawCards)
        $ FastAbility (assetUseCost a Secret 2)
    , restrictedAbility a 3 (ControlsThis <> CanDiscoverCluesAt Anywhere)
        $ FastAbility (assetUseCost a Secret 3)
    , withCriteria (mkAbility a 4 $ FastAbility $ assetUseCost a Secret 4)
        $ ControlsThis <> enemyExists (EnemyIsEngagedWith You <> EnemyCanBeDamagedBySource (toSource a))
    ]

instance RunMessage TheNecronomiconPetrusDeDaciaTranslation5 where
  runMessage msg a@(TheNecronomiconPetrusDeDaciaTranslation5 attrs) =
    case msg of
      UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
        push $ skillTestModifier (toAbilitySource attrs 1) iid (AnySkillValue 2)
        pure a
      UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
        pushM $ drawCards iid (toAbilitySource attrs 2) 2
        pure a
      UseCardAbility iid (isSource attrs -> True) 3 _ _ -> do
        lids <- selectList $ LocationWithDiscoverableCluesBy $ InvestigatorWithId iid
        pushWhen (notNull lids)
          $ chooseOrRunOne
            iid
            [ targetLabel lid [DiscoverCluesAtLocation iid lid (toAbilitySource attrs 1) 2 Nothing]
            | lid <- lids
            ]
        pure a
      UseCardAbility iid (isSource attrs -> True) 4 _ _ -> do
        eids <- selectList $ enemyEngagedWith iid <> EnemyCanBeDamagedBySource (toSource attrs)
        push
          $ chooseOrRunOne
            iid
            [ targetLabel eid [EnemyDamage eid $ nonAttack attrs 3]
            | eid <- eids
            ]
        pure a
      _ -> TheNecronomiconPetrusDeDaciaTranslation5 <$> runMessage msg attrs
