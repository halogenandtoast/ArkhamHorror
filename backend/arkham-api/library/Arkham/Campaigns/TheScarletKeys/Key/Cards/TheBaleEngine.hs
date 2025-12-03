module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheBaleEngine (theBaleEngine) where

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted
import Arkham.I18n
import Arkham.Matcher hiding (key)
import Arkham.Message.Lifted.Choose

newtype TheBaleEngine = TheBaleEngine ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBaleEngine :: ScarletKeyCard TheBaleEngine
theBaleEngine = key TheBaleEngine Cards.theBaleEngine

instance HasAbilities TheBaleEngine where
  getAbilities (TheBaleEngine a) = case a.bearer of
    InvestigatorTarget iid
      | not a.shifted ->
          if a.stable
            then
              [ restricted
                  a
                  1
                  ( youExist (InvestigatorWithId iid)
                      <> exists
                        ( AssetControlledBy (affectsColocated iid)
                            <> mapOneOf AssetCanHaveUses [Ammo, Charge, Secret, Supply, Evidence]
                        )
                  )
                  $ FastAbility Free
              ]
            else
              [restricted a 1 (youExist (InvestigatorWithId iid) <> exists InvestigatorWithAnyResources) $ FastAbility Free]
    _ -> []

instance RunMessage TheBaleEngine where
  runMessage msg k@(TheBaleEngine attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09769]" Null) k
    CampaignSpecific "shift[09769]" _ -> do
      shiftKey attrs do
        when attrs.unstable do
          investigators <- select InvestigatorWithAnyResources
          for_ investigators $ loseResourcesOf attrs 3
          unless (null investigators) $ withInvestigatorBearer attrs (`flipOver` attrs)
        when attrs.stable do
          withInvestigatorBearer attrs \iid -> do
            assets <-
              select
                $ AssetControlledBy (affectsColocated iid)
                <> mapOneOf AssetCanHaveUses [Ammo, Charge, Secret, Supply, Evidence]
            chooseTargetM iid assets $ handleTarget iid attrs
            flipOver iid attrs
      pure k
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      canAmmo <- matches aid (AssetCanHaveUses Ammo)
      canCharge <- matches aid (AssetCanHaveUses Charge)
      canSecret <- matches aid (AssetCanHaveUses Secret)
      canSupply <- matches aid (AssetCanHaveUses Supply)
      canEvidence <- matches aid (AssetCanHaveUses Evidence)

      chooseOrRunOneM iid $ withI18n do
        when canAmmo $ labeled' "ammo" $ addUses attrs aid Ammo 1
        when canCharge $ labeled' "charge" $ addUses attrs aid Charge 1
        when canSecret $ labeled' "secret" $ addUses attrs aid Secret 1
        when canSupply $ labeled' "supply" $ addUses attrs aid Supply 1
        when canEvidence $ labeled' "evidence" $ addUses attrs aid Evidence 1
      pure k
    _ -> TheBaleEngine <$> liftRunMessage msg attrs
