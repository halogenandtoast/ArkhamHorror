module Arkham.Asset.Cards.SurgicalKit3 (surgicalKit3, SurgicalKit3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator (canHaveHorrorHealed, healAdditional)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Matcher

newtype SurgicalKit3 = SurgicalKit3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

surgicalKit3 :: AssetCard SurgicalKit3
surgicalKit3 = asset SurgicalKit3 Cards.surgicalKit3

instance HasAbilities SurgicalKit3 where
  getAbilities (SurgicalKit3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          ( InvestigatorHealed #when #damage (affectsOthers Anyone) (SourceOwnedBy You <> SourceIsCardEffect)
          )
          (assetUseCost a Supply 1)
    , controlledAbility a 2 criteria $ ActionAbility [] (ActionCost 2)
    ]
   where
    criteria =
      oneOf
        [ exists $ HealableInvestigator (a.ability 2) #damage $ at_ YourLocation
        , exists $ HealableAsset (a.ability 2) #damage $ at_ YourLocation
        ]

instance RunMessage SurgicalKit3 where
  runMessage msg a@(SurgicalKit3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mDraw <- Msg.drawCardsIfCan iid (attrs.ability 1) 1
      canHeal <- canHaveHorrorHealed (attrs.ability 1) iid
      chooseOne
        iid
        $ Label "That effect heals 1 additional damage" [DoStep 1 msg]
        : [ Label
            "Draw 1 card and heal 1 horror"
            (maybeToList mDraw <> [HealHorror (toTarget iid) (attrs.ability 1) 1 | canHeal])
          | isJust mDraw || canHeal
          ]
      pure a
    DoStep 1 (UseCardAbility _iid (isSource attrs -> True) 1 ws' _) -> do
      healAdditional (attrs.ability 1) #damage ws' 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      investigatorChoices <- select $ HealableInvestigator (attrs.ability 2) #damage (colocatedWith iid)
      assetChoices <-
        select $ HealableAsset (attrs.ability 2) #damage $ at_ (locationWithInvestigator iid)
      when (notNull investigatorChoices || notNull assetChoices) $ do
        chooseOne iid
          $ [targetLabel iid' [HealDamage (toTarget iid') (attrs.ability 2) 3] | iid' <- investigatorChoices]
          <> [targetLabel aid [HealDamage (toTarget aid) (attrs.ability 2) 3] | aid <- assetChoices]
      pure a
    _ -> SurgicalKit3 <$> liftRunMessage msg attrs
