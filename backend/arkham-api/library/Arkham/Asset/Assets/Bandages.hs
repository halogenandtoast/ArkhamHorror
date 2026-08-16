module Arkham.Asset.Assets.Bandages (bandages) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Bandages = Bandages AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bandages :: AssetCard Bandages
bandages = assetWith Bandages Cards.bandages discardWhenNoUses

instance HasAbilities Bandages where
  getAbilities (Bandages a) =
    [ controlled a 1 (thisExists a $ AssetWithUses Supply)
        $ triggered
          ( oneOf
              [ AssetDealtDamage
                  #after
                  AnySource
                  (HealableAsset (toSource a) #damage $ AssetAt YourLocation <> #ally)
              , -- Matcher.DealtDamage matches the aggregate TakeDamage window *and* the
                -- per-recipient DealtDamage one, so the ability is offered whenever the
                -- investigator took anything (an Ally soak included, per FAQ 2.12) while
                -- still attaching the per-recipient window getHealTargets needs.
                DealtDamage
                  #after
                  AnySource
                  (HealableInvestigator (toSource a) #damage $ colocatedWithMatch You)
              ]
          )
          Free
    ]

-- Only the per-recipient DealtDamage windows: the aggregate TakeDamage window on the
-- investigator also covers damage soaked by an Ally they control (FAQ 2.12), so reading
-- it here would offer to heal an investigator who took nothing (#5394).
getHealTargets :: [Window] -> [Target]
getHealTargets = \case
  (windowType -> Window.DealtDamage _ _ target _) : rest -> target : getHealTargets rest
  _ : rest -> getHealTargets rest
  [] -> []

instance RunMessage Bandages where
  runMessage msg a@(Bandages attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getHealTargets -> rawTargets) _ -> do
      let source = attrs.ability 1
      -- Bandages triggers once per damaged investigator/Ally, not per point of
      -- damage, so reduce to the distinct, currently-healable entities that took
      -- damage this window.
      healableInvestigators <-
        selectMap InvestigatorTarget $ HealableInvestigator source #damage (colocatedWith iid)
      healableAllies <-
        selectMap AssetTarget $ HealableAsset source #damage (at_ (locationWithInvestigator iid) <> #ally)
      let healTargets = filter (`elem` (healableInvestigators <> healableAllies)) (nub rawTargets)
      supplies <- findWithDefault 0 Supply <$> field AssetUses attrs.id
      chooseUpToNM_ iid (min supplies (length healTargets)) do
        targets healTargets \target -> do
          push $ SpendUses source (toTarget attrs) Supply 1
          healDamage target source 1
      pure a
    _ -> Bandages <$> liftRunMessage msg attrs
