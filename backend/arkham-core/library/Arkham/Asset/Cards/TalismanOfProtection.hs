module Arkham.Asset.Cards.TalismanOfProtection (
  talismanOfProtection,
  TalismanOfProtection (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Classes.HasQueue (replaceMessageMatching)
import Arkham.Helpers.Message (checkDefeated)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype TalismanOfProtection = TalismanOfProtection AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

talismanOfProtection :: AssetCard TalismanOfProtection
talismanOfProtection = asset TalismanOfProtection Cards.talismanOfProtection

instance HasAbilities TalismanOfProtection where
  getAbilities (TalismanOfProtection x) =
    [ controlledAbility
        x
        1
        (exists $ oneOf [HealableInvestigator ThisCard dType You | dType <- [#damage, #horror]])
        $ ReactionAbility
          ( InvestigatorWouldBeDefeated
              #when
              (BySource (SourceIsCancelable AnySource) <> ByAnyOf [ByHorror, ByDamage])
              You
          )
          (discardCost x)
    ]

instance RunMessage TalismanOfProtection where
  runMessage msg a@(TalismanOfProtection attrs) = runQueueT $ case msg of
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      owner <- field AssetOwner (toId attrs)
      when (Just iid == owner) $ do
        iids <- select $ affectsOthers $ colocatedWith iid
        chooseOrRunOne iid $ targetLabels iids $ only . (`TakeControlOfAsset` (toId attrs))
      TalismanOfProtection <$> runMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      replaceMessageMatching
        \case
          InvestigatorWhenDefeated _ iid' -> iid == iid'
          _ -> False
        \case
          InvestigatorWhenDefeated source' _ -> [checkDefeated source' iid]
          _ -> error "invalid match"

      push $ DoStep 2 msg
      pure a
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      h <- field InvestigatorAssignedHorror iid
      d <- field InvestigatorAssignedDamage iid
      chooseOrRunOne iid
        $ Label "Done canceling" []
        : [HorrorLabel iid [CancelAssignedDamage (toTarget iid) 0 1, DoStep (n - 1) msg'] | h > 0]
          <> [DamageLabel iid [CancelAssignedDamage (toTarget iid) 1 0, DoStep (n - 1) msg'] | d > 0]
      pure a
    _ -> TalismanOfProtection <$> lift (runMessage msg attrs)
