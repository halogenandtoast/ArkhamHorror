module Arkham.Event.Events.Absolution (absolution) where

import Arkham.ChaosToken
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.ChaosBag
import Arkham.Matcher

newtype Absolution = Absolution EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

absolution :: EventCard Absolution
absolution = event Absolution Cards.absolution

instance RunMessage Absolution where
  runMessage msg e@(Absolution attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let x = totalResourcePayment attrs.payment
      requestChaosTokens iid attrs (3 + x)
      pure e
    RequestedChaosTokens (isSource attrs -> True) _ tokens -> do
      let nonSymbols = count (not . isSymbolChaosToken . (.face)) tokens
      availableBless <- getRemainingBlessTokens
      repeated (min nonSymbols availableBless) $ addChaosToken #bless

      let bless = count ((== #bless) . (.face)) tokens
      doStep bless msg
      pure e
    DoStep n msg'@(RequestedChaosTokens (isSource attrs -> True) (Just iid) _) | n > 0 -> do
      assets <-
        selectTargets $ HealableAsset (toSource attrs) #horror $ AssetAt (locationWithInvestigator iid)
      investigators <- selectTargets $ HealableInvestigator (toSource attrs) #horror $ colocatedWith iid

      when (notNull assets || notNull investigators) do
        chooseOneM iid do
          targets (assets <> investigators) \t -> do
            healHorror t attrs 1
            doStep (n - 1) msg'
      pure e
    _ -> Absolution <$> liftRunMessage msg attrs
