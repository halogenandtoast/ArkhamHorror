module Arkham.Homebrew.AgesUnwound.Treacheries.AcceleratedDecay (acceleratedDecay) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted hiding (RevealChaosToken)

newtype AcceleratedDecay = AcceleratedDecay TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acceleratedDecay :: TreacheryCard AcceleratedDecay
acceleratedDecay = treachery AcceleratedDecay Cards.acceleratedDecay

{- | "[free]: Discard attached asset." / "Forced - After you reveal a [skull]
token during a skill test: Place 1 resource on Accelerated Decay, then take 1
damage for each resource on Accelerated Decay."
-}
instance HasAbilities AcceleratedDecay where
  getAbilities (AcceleratedDecay a) =
    [ restricted a 1 (youExist $ be a.drawnBy) $ FastAbility Free
    , restricted a 2 (youExist $ be a.drawnBy) $ forced $ RevealChaosToken #after You #skull
    ]

{- | "Revelation - Attach Accelerated Decay to an asset you control. (If you
control no assets, take 2 damage.)"
-}
instance RunMessage AcceleratedDecay where
  runMessage msg t@(AcceleratedDecay attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <- select $ assetControlledBy iid
      if null assets
        then do
          assignDamage iid attrs 2
          toDiscard attrs attrs
        else chooseOrRunOneM iid $ targets assets $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attached \case
        AssetTarget aid -> toDiscardBy iid (attrs.ability 1) aid
        _ -> pure ()
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      placeTokens (attrs.ability 2) attrs #resource 1
      -- the resource just placed is not on the card yet, so count it here
      assignDamage iid (attrs.ability 2) (attrs.resources + 1)
      pure t
    _ -> AcceleratedDecay <$> liftRunMessage msg attrs
