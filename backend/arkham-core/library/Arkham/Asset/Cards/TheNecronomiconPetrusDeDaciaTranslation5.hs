module Arkham.Asset.Cards.TheNecronomiconPetrusDeDaciaTranslation5 (
  theNecronomiconPetrusDeDaciaTranslation5,
  TheNecronomiconPetrusDeDaciaTranslation5 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.DamageEffect
import Arkham.Discover
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
    [ withTooltip "{fast} Spend 1 secret: You get +2 skill value for this skill test."
        $ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility (assetUseCost a Secret 1)
    , withTooltip "{fast} Spend 2 secrets: Draw 2 cards."
        $ restrictedAbility a 2 (ControlsThis <> CanDrawCards)
        $ FastAbility (assetUseCost a Secret 2)
    , withTooltip "{fast} Spend 3 secrets: Discover 1 clue at any location."
        $ restrictedAbility a 3 (ControlsThis <> CanDiscoverCluesAt Anywhere)
        $ FastAbility (assetUseCost a Secret 3)
    , withTooltip "{fast} Spend 4 secrets: Deal 3 damage to an enemy engaged with you."
        $ withCriteria (mkAbility a 4 $ FastAbility $ assetUseCost a Secret 4)
        $ ControlsThis
        <> enemyExists (EnemyIsEngagedWith You <> EnemyCanBeDamagedBySource (toSource a))
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
        lids <- select $ LocationWithDiscoverableCluesBy $ InvestigatorWithId iid
        player <- getPlayer iid
        pushWhen (notNull lids)
          $ chooseOrRunOne
            player
            [ targetLabel lid [toMessage $ discover iid lid (attrs.ability 1) 2]
            | lid <- lids
            ]
        pure a
      UseCardAbility iid (isSource attrs -> True) 4 _ _ -> do
        eids <- select $ enemyEngagedWith iid <> EnemyCanBeDamagedBySource (toSource attrs)
        player <- getPlayer iid
        push
          $ chooseOrRunOne
            player
            [ targetLabel eid [EnemyDamage eid $ nonAttack attrs 3]
            | eid <- eids
            ]
        pure a
      _ -> TheNecronomiconPetrusDeDaciaTranslation5 <$> runMessage msg attrs
