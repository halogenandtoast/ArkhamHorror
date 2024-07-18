module Arkham.Asset.Cards.HypnoticTherapy (hypnoticTherapy, HypnoticTherapy (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator (healAdditional)
import Arkham.Matcher
import Arkham.Prelude

newtype HypnoticTherapy = HypnoticTherapy AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypnoticTherapy :: AssetCard HypnoticTherapy
hypnoticTherapy = asset HypnoticTherapy Cards.hypnoticTherapy

instance HasAbilities HypnoticTherapy where
  getAbilities (HypnoticTherapy a) =
    [ restrictedAbility a 1 ControlsThis $ actionAbilityWithCost (exhaust a)
    , restrictedAbility a 2 ControlsThis
        $ ReactionAbility
          ( InvestigatorHealed #after #horror (affectsOthers Anyone)
              $ SourceOwnedBy You
              <> NotSource (SourceIs (toSource a))
          )
        $ exhaust a
    ]

instance RunMessage HypnoticTherapy where
  runMessage msg a@(HypnoticTherapy attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid (attrs.ability 1) iid #intellect (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      targetsWithCardDrawAndHeal <- do
        iids <- select $ HealableInvestigator (attrs.ability 1) #horror $ colocatedWith iid
        for iids $ \i -> do
          let draw = drawCards i (toAbilitySource attrs 1) 1
          targetPlayer <- getPlayer i
          pure ((i, targetPlayer), draw, HealHorror (toTarget i) (attrs.ability 1) 1)
      player <- getPlayer iid
      pushWhen (notNull targetsWithCardDrawAndHeal)
        $ chooseOrRunOne player
        $ [ targetLabel target
            $ [ heal
              , chooseOne
                  targetPlayer
                  [Label "Do Not Draw" [], ComponentLabel (InvestigatorDeckComponent target) [drawing]]
              ]
          | ((target, targetPlayer), drawing, heal) <- targetsWithCardDrawAndHeal
          ]
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws' _ -> do
      healAdditional (attrs.ability 2) #horror ws' 1
      pure a
    _ -> HypnoticTherapy <$> runMessage msg attrs
