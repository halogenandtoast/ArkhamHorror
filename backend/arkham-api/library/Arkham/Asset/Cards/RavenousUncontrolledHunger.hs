module Arkham.Asset.Cards.RavenousUncontrolledHunger (
  ravenousUncontrolledHunger,
  RavenousUncontrolledHunger (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Types (getController)
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection

newtype RavenousUncontrolledHunger = RavenousUncontrolledHunger AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ravenousUncontrolledHunger :: AssetCard RavenousUncontrolledHunger
ravenousUncontrolledHunger = asset RavenousUncontrolledHunger Cards.ravenousUncontrolledHunger

instance HasAbilities RavenousUncontrolledHunger where
  getAbilities (RavenousUncontrolledHunger a) = [restrictedAbility a 1 ControlsThis $ forced $ TurnEnds #when You]

instance HasModifiersFor RavenousUncontrolledHunger where
  getModifiersFor (InvestigatorTarget iid) (RavenousUncontrolledHunger a) | a `controlledBy` iid = do
    n <- fieldMap AssetCardsUnderneath (min 5 . length) a.id
    pure $ toModifiers a [SkillModifier sType n | sType <- [minBound ..]]
  getModifiersFor _ _ = pure []

instance RunMessage RavenousUncontrolledHunger where
  runMessage msg a@(RavenousUncontrolledHunger attrs) = runQueueT $ case msg of
    Devoured _iid card -> do
      pure . RavenousUncontrolledHunger $ attrs & cardsUnderneathL %~ filter (/= card)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      devourable <-
        select
          $ ControlledBy (affectsOthers $ colocatedWith iid)
          <> basic (not_ (CardWithType StoryType) <> not_ PermanentCard)
      underneath <- fieldMap AssetCardsUnderneath (filterCards (not_ (CardWithType StoryType))) attrs.id
      chooseOneM iid do
        for_ underneath \card -> do
          targeting card do
            push $ Devoured iid card

        for_ devourable \card -> do
          selectOne (AssetWithCardId card.id <> AssetCanLeavePlayByNormalMeans) >>= traverse_ \asset' -> do
            targeting card do
              push $ RemoveFromPlay (toSource asset')
              push $ Devoured iid card
          selectOne (EventWithCardId card.id) >>= traverse_ \event -> do
            targeting card do
              push $ RemoveFromPlay (toSource event)
              push $ Devoured iid card
          selectOne (SkillWithCardId card.id) >>= traverse_ \skill -> do
            targeting card do
              push $ RemoveFromPlay (toSource skill)
              push $ Devoured iid card
      doStep 1 msg
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      pushWhen (null $ assetCardsUnderneath attrs)
        $ Flip iid (attrs.ability 1) (toTarget attrs)
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      let
        ravenousControlledHunger =
          PlayerCard
            $ lookupPlayerCard Cards.ravenousControlledHunger (toCardId attrs)
        subjectId = getController attrs
      push $ ReplaceInvestigatorAsset subjectId attrs.id ravenousControlledHunger
      pure $ RavenousUncontrolledHunger $ attrs & flippedL .~ True
    _ -> RavenousUncontrolledHunger <$> liftRunMessage msg attrs
