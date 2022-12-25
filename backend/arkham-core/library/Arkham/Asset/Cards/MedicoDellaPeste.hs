module Arkham.Asset.Cards.MedicoDellaPeste
  ( medicoDellaPeste
  , MedicoDellaPeste(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding ( InvestigatorDamage )
import Arkham.Cost
import Arkham.Criteria
import Arkham.Damage
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype MedicoDellaPeste = MedicoDellaPeste AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

medicoDellaPeste :: AssetCard MedicoDellaPeste
medicoDellaPeste = asset MedicoDellaPeste Cards.medicoDellaPeste

instance HasAbilities MedicoDellaPeste where
  getAbilities (MedicoDellaPeste a) =
    [ restrictedAbility
        a
        1
        (ControlsThis <> InvestigatorExists
          (AnyInvestigator
            [ HealableInvestigator DamageType You
            , HealableInvestigator HorrorType You
            ]
          )
        )
      $ ReactionAbility
          (AssetEntersPlay Timing.After $ AssetWithId $ toId a)
          Free
    , restrictedAbility a 2 ControlsThis $ ReactionAbility
      (InitiatedSkillTest
        Timing.When
        You
        (NotSkillType SkillWillpower)
        AnySkillTestValue
      )
      (DiscardCost $ toTarget a)
    ]

instance RunMessage MedicoDellaPeste where
  runMessage msg a@(MedicoDellaPeste attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      hasDamage <-
        selectAny $ HealableInvestigator DamageType $ InvestigatorWithId iid
      hasHorror <-
        selectAny $ HealableInvestigator HorrorType $ InvestigatorWithId iid
      push
        $ chooseOrRunOne iid
        $ [ Label
              "Heal 1 damage"
              [HealDamage (InvestigatorTarget iid) (toSource attrs) 1]
          | hasDamage
          ]
        <> [ Label
               "Heal 1 horror"
               [HealHorror (InvestigatorTarget iid) (toSource attrs) 1]
           | hasHorror
           ]
      pure a
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      replaceMessageMatching
        (\case
          BeginSkillTestAfterFast{} -> True
          Ask _ (ChooseOne (SkillLabel _ (BeginSkillTestAfterFast{} : _) : _))
            -> True
          _ -> False
        )
        (\case
          BeginSkillTestAfterFast iid' source' target' maction' _ difficulty'
            -> [ BeginSkillTest
                   iid'
                   source'
                   target'
                   maction'
                   SkillWillpower
                   difficulty'
               ]
          Ask _ (ChooseOne (SkillLabel _ (BeginSkillTestAfterFast iid' source' target' maction' _ difficulty' : _) : _))
            -> [ BeginSkillTest
                   iid'
                   source'
                   target'
                   maction'
                   SkillWillpower
                   difficulty'
               ]
          _ -> error "invalid match"
        )
      pure a
    _ -> MedicoDellaPeste <$> runMessage msg attrs
