module Arkham.Event.Events.KnowledgeIsPower (knowledgeIsPower) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Ability
import Arkham.Helpers.Card
import Arkham.Helpers.Modifiers
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Window qualified as Window

newtype KnowledgeIsPower = KnowledgeIsPower EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knowledgeIsPower :: EventCard KnowledgeIsPower
knowledgeIsPower = event KnowledgeIsPower Cards.knowledgeIsPower

cardMatcher :: ExtendedCardMatcher
cardMatcher =
  basic (oneOf [#tome, #spell] <> #asset)
    <> CardWithPerformableAbility (oneOf [AbilityIsActionAbility, AbilityIsFastAbility]) [IgnoreAllCosts]

instance RunMessage KnowledgeIsPower where
  runMessage msg e@(KnowledgeIsPower attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      assets <-
        select
          $ assetControlledBy iid
          <> oneOf [#spell, #tome]
          <> AssetWithPerformableAbility (oneOf [AbilityIsActionAbility, AbilityIsFastAbility]) [IgnoreAllCosts]

      cards <- fieldMapM InvestigatorHand (filterM (`extendedCardMatch` cardMatcher)) iid

      chooseOneM iid do
        targets assets $ handleTarget iid attrs
        targets cards \card -> do
          withCardEntity @AssetId card $ handleTarget iid attrs
          forTarget card.id msg
      pure e
    ForTarget (CardIdTarget cid) (PlayThisEvent iid (is attrs -> True)) -> do
      inHand <- selectMap toCardId $ InHandOf NotForPlay (InvestigatorWithId iid)
      when (cid `elem` inHand) do
        chooseOneM iid do
          cardI18n $ scope "knowledgeIsPower" $ labeled' "discardToDraw" do
            push $ DiscardCard iid (toSource attrs) cid
            drawCards iid attrs 1
          labeledI "doNotDiscard" nothing
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      let adjustAbility ab = applyAbilityModifiers (noAOO ab) [IgnoreAllCosts]
      abilities <-
        selectMap adjustAbility
          $ AssetAbility (AssetWithId aid)
          <> oneOf [AbilityIsActionAbility, AbilityIsFastAbility]
      abilities' <- filterM (getCanPerformAbility iid (Window.defaultWindows iid)) abilities
      chooseOneM iid $ for_ abilities' \ab -> abilityLabeled iid ab nothing
      pure e
    _ -> KnowledgeIsPower <$> liftRunMessage msg attrs
