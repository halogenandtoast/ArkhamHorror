module Arkham.Asset.Cards.MedicoDellaPeste (
  medicoDellaPeste,
  MedicoDellaPeste (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (InvestigatorDamage)
import Arkham.Damage
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.SkillTest
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype MedicoDellaPeste = MedicoDellaPeste AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

medicoDellaPeste :: AssetCard MedicoDellaPeste
medicoDellaPeste = asset MedicoDellaPeste Cards.medicoDellaPeste

instance HasAbilities MedicoDellaPeste where
  getAbilities (MedicoDellaPeste a) =
    [ restrictedAbility
        a
        1
        ( ControlsThis
            <> InvestigatorExists
              ( AnyInvestigator
                  [ HealableInvestigator (toSource a) DamageType You
                  , HealableInvestigator (toSource a) HorrorType You
                  ]
              )
        )
        $ ReactionAbility
          (AssetEntersPlay Timing.After $ AssetWithId $ toId a)
          Free
    , restrictedAbility a 2 ControlsThis
        $ ReactionAbility
          ( InitiatedSkillTest
              Timing.When
              You
              (NotSkillType SkillWillpower)
              AnySkillTestValue
              #any
          )
          (DiscardCost FromPlay $ toTarget a)
    ]

instance RunMessage MedicoDellaPeste where
  runMessage msg a@(MedicoDellaPeste attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      hasDamage <- canHaveDamageHealed attrs iid
      mHealHorror <- getHealHorrorMessage attrs 1 iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ Label
            "Heal 1 damage"
            [HealDamage (InvestigatorTarget iid) (toSource attrs) 1]
          | hasDamage
          ]
        <> [ Label "Heal 1 horror" [healHorror]
           | healHorror <- maybeToList mHealHorror
           ]
      pure a
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      skillTest <- getJustSkillTest
      let
        newBase =
          case skillTestBaseValue skillTest of
            SkillBaseValue _ -> SkillBaseValue #willpower
            AndSkillBaseValue _ -> SkillBaseValue #willpower
            HalfResourcesOf x -> HalfResourcesOf x
            StaticBaseValue x -> StaticBaseValue x
      push $ ChangeSkillTestType (SkillSkillTest #willpower) newBase
      pure a
    _ -> MedicoDellaPeste <$> runMessage msg attrs
