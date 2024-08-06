module Arkham.Investigator.Cards.Subject5U21 (subject5U21, Subject5U21 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Draw.Types
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Helpers.Modifiers (ModifierType (..), hasModifier)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Runner (takeUpkeepResources, pattern PlayThisEvent)
import Arkham.Investigator.Types (Field (..), defeatedL, resignedL)
import Arkham.Matcher hiding (AssetCard, PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype Meta = Meta {devoured :: [Card]}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype Subject5U21 = Subject5U21 (InvestigatorAttrs `With` Meta)
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

subject5U21 :: InvestigatorCard Subject5U21
subject5U21 =
  startsWith [Assets.ravenousControlledHunger]
    $ investigator (Subject5U21 . (`with` Meta [])) Cards.subject5U21
    $ Stats {health = 6, sanity = 6, willpower = 1, intellect = 1, combat = 1, agility = 1}

instance HasAbilities Subject5U21 where
  getAbilities (Subject5U21 (With a _)) =
    [ playerLimit PerRound
        $ restrictedAbility
          a
          1
          ( Self
              <> exists
                ( ControlledBy (affectsOthers $ InvestigatorAt YourLocation)
                    <> basic (not_ (CardWithType StoryType) <> not_ PermanentCard)
                )
          )
        $ FastAbility Free
    ]

instance HasChaosTokenValue Subject5U21 where
  getChaosTokenValue iid ElderSign (Subject5U21 (With attrs _)) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage Subject5U21 where
  runMessage msg i@(Subject5U21 (With attrs meta)) = runQueueT $ case msg of
    AllDrawCardAndResource | not (attrs ^. defeatedL || attrs ^. resignedL) -> do
      unlessM (hasModifier attrs CannotDrawCards) $ do
        push $ DrawCards attrs.id $ newCardDraw ScenarioSource attrs.id 2
        push $ Devour attrs.id
      Subject5U21 . (`with` meta) <$> takeUpkeepResources attrs
    ElderSignEffect (is attrs -> True) -> do
      afterSkillTest $ push (Do msg)
      pure i
    Do (ElderSignEffect iid) -> do
      mRavenous <-
        selectOne
          (oneOf [assetIs Assets.ravenousControlledHunger, assetIs Assets.ravenousUncontrolledHunger])
      devouredRavenous <- maybe (pure []) (field AssetCardsUnderneath) mRavenous

      let allDevoured = devouredRavenous <> devoured meta

      focusCards allDevoured \unfocus -> do
        chooseOneM iid do
          labeled "Do not return a devoured card" nothing
          for_ allDevoured \case
            card@(PlayerCard pc) -> for_ (pcOwner pc) \owner ->
              targeting card $ addToHand owner [card]
            _ -> pure ()
        push unfocus
      pure i
    AddToHand _ cards -> do
      attrs' <- liftRunMessage msg attrs
      pure $ Subject5U21 $ attrs' `with` Meta (filter (`notElem` cards) $ devoured meta)
    Devour iid | iid == toId attrs -> do
      devourable <- fieldMap InvestigatorHand (filterCards NonWeakness) attrs.id
      unless (null devourable) do
        chooseOneM iid do
          for_ devourable \card ->
            targeting card $ push $ Devoured iid card
      pure i
    Devoured iid card | iid == toId attrs -> do
      send $ format attrs <> " devours " <> format card
      selectOne (assetIs Assets.ravenousControlledHunger) >>= \case
        Nothing -> pure . Subject5U21 $ attrs `with` Meta (card : devoured meta)
        Just aid -> do
          push $ PlaceUnderneath (toTarget aid) [card]
          pure i
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      devourable <-
        select
          $ ControlledBy (affectsOthers $ colocatedWith iid)
          <> basic (not_ (CardWithType StoryType) <> not_ PermanentCard)
      chooseOneM iid do
        for_ devourable \card -> do
          selectOne (AssetWithCardId card.id <> AssetCanLeavePlayByNormalMeans) >>= traverse_ \asset -> do
            targeting card do
              push $ RemoveFromPlay (toSource asset)
              push $ Devoured iid card
          selectOne (EventWithCardId card.id) >>= traverse_ \event -> do
            targeting card do
              push $ RemoveFromPlay (toSource event)
              push $ Devoured iid card
          selectOne (SkillWithCardId card.id) >>= traverse_ \skill -> do
            targeting card do
              push $ RemoveFromPlay (toSource skill)
              push $ Devoured iid card
      pure i
    ForTarget (isTarget attrs -> True) (PlayThisEvent _ _) -> do
      -- Regurgitation

      mRavenous <-
        selectOne
          (oneOf [assetIs Assets.ravenousControlledHunger, assetIs Assets.ravenousUncontrolledHunger])
      devouredRavenous <- maybe (pure []) (field AssetCardsUnderneath) mRavenous

      let allDevoured = devouredRavenous <> devoured meta

      focusCards allDevoured \unfocus -> do
        chooseUpToNM attrs.id 3 "Do not regurgitate any more cards" do
          for_ allDevoured \case
            card@(PlayerCard pc) -> for_ (pcOwner pc) \owner ->
              targeting card do
                addToHand owner [card]
                doStep 1 msg
            _ -> pure ()
        push unfocus

      pure i
    DoStep 1 (ForTarget (isTarget attrs -> True) (PlayThisEvent _ eid)) -> do
      canHealDamage <- canHaveDamageHealed eid attrs.id
      canHealHorror <- canHaveHorrorHealed eid attrs.id
      when (canHealHorror || canHealDamage) do
        chooseOrRunOneM attrs.id do
          when canHealDamage do
            labeled "Heal 1 damage" $ healDamage attrs eid 1
          when canHealHorror do
            labeled "Heal 1 horror" $ healHorror attrs eid 1
      pure i
    _ -> Subject5U21 . (`with` meta) <$> liftRunMessage msg attrs
