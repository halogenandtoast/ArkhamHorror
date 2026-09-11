module Arkham.Homebrew.CircusExMortis.Treacheries.CloseWatch (closeWatch) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Cost (getCanAffordCost, payEffectCost)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Import.Lifted hiding (PerformAction)

newtype CloseWatch = CloseWatch TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeWatch :: TreacheryCard CloseWatch
closeWatch = treachery CloseWatch Cards.closeWatch

instance HasAbilities CloseWatch where
  getAbilities (CloseWatch a) =
    [ mkAbility a 1 $ forced $ RoundEnds #when
    , mkAbility a 2 $ forced $ PerformAction #when You #move
    ]

instance RunMessage CloseWatch where
  runMessage msg t@(CloseWatch attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- The move's own action has not been taken yet, so paying 1 additional
      -- means affording both.
      canSpend <- getCanAffordCost iid (attrs.ability 2) [#move] [] (ActionCost 2)
      chooseOrRunOneM iid $ campaignI18n $ scope "closeWatch" do
        when canSpend $ labeled "spendAdditionalAction" do
          payEffectCost iid attrs (ActionCost 1)
        labeled "discardUntilEnemy" do
          afterMove attrs iid $ discardUntilFirst iid attrs Deck.EncounterDeck (basic #enemy)
      pure t
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just card) -> do
      createEnemyAtLocationMatching_ card (locationWithInvestigator iid)
      pure t
    _ -> CloseWatch <$> liftRunMessage msg attrs
