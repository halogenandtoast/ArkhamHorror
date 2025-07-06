module Arkham.Asset.Assets.HyperphysicalShotcasterTheoreticalDevice (
  hyperphysicalShotcasterTheoreticalDevice,
)
where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (afterMove)
import Arkham.Card
import Arkham.Customization
import Arkham.Evade qualified as Evade
import Arkham.Fight qualified as Fight
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Customization
import Arkham.Helpers.Investigator
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifySelf)
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Investigate qualified as Investigate
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Movement hiding (moveToMatch)
import Arkham.SkillType
import Arkham.Token
import Arkham.Window (defaultWindows)

newtype Metadata = Metadata {manifest :: Maybe Customization}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HyperphysicalShotcasterTheoreticalDevice
  = HyperphysicalShotcasterTheoreticalDevice (With AssetAttrs Metadata)
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hyperphysicalShotcasterTheoreticalDevice :: AssetCard HyperphysicalShotcasterTheoreticalDevice
hyperphysicalShotcasterTheoreticalDevice =
  assetWith
    (HyperphysicalShotcasterTheoreticalDevice . (`with` Metadata Nothing))
    Cards.hyperphysicalShotcasterTheoreticalDevice
    discardWhenNoUses

instance HasModifiersFor HyperphysicalShotcasterTheoreticalDevice where
  getModifiersFor (HyperphysicalShotcasterTheoreticalDevice (With attrs meta)) = do
    self <-
      if attrs `hasCustomization` AethericLink
        then modifySelf attrs [AdditionalStartingUses 2]
        else pure mempty

    ability <- case manifest meta of
      Just Translocator -> do
        case attrs.controller of
          Nothing -> pure mempty
          Just iid ->
            selectOne (AbilityIs (toSource attrs) 1) >>= \case
              Nothing -> pure mempty
              Just ab ->
                modified_
                  attrs
                  (AbilityTarget iid ab.ref)
                  [ CanModify
                      $ EnemyEvadeActionCriteria
                      $ CriteriaOverride
                      $ EnemyCriteria
                      $ ThisEnemy
                      $ oneOf
                        [ EnemyAt (ConnectedFrom YourLocation)
                            <> NonEliteEnemy
                            <> EnemyWithEvade
                            <> EnemyCanEnter YourLocation
                        , EnemyAt (CanEnterLocation You)
                            <> oneOf [not_ (EnemyIsEngagedWith Anyone), MassiveEnemy]
                            <> EnemyWithEvade
                        , EnemyAt YourLocation <> EnemyIsEngagedWith You <> EnemyWithEvade
                        ]
                  ]
      _ -> pure mempty
    pure $ self <> ability

instance HasAbilities HyperphysicalShotcasterTheoreticalDevice where
  getAbilities (HyperphysicalShotcasterTheoreticalDevice (With x meta)) =
    ( case manifest meta of
        Nothing -> []
        Just c -> [restrictedAbility x 1 (ControlsThis <> manifestCriteria c) (manifestAbility c)]
    )
      <> [restrictedAbility x 2 (ControlsThis <> canChangeManifest) $ FastAbility (exhaust x)]
   where
    canChangeManifest =
      if any
        (\c -> x `hasCustomization` c && manifest meta /= Just c)
        [Railshooter, Telescanner, Translocator, Realitycollapser, Matterweaver]
        then NoRestriction
        else Never
    manifestAbility = \case
      Railshooter -> fightAction (assetUseCost x Aether 1)
      Telescanner -> investigateAction (assetUseCost x Aether 1)
      Translocator -> evadeAction (assetUseCost x Aether 1)
      Realitycollapser -> actionAbilityWithCost (assetUseCost x Aether 1)
      Matterweaver -> actionAbilityWithCost (assetUseCost x Aether 1)
      _ -> error "Invalid manifest ability"
    manifestCriteria = \case
      Railshooter -> NoRestriction
      Telescanner -> NoRestriction
      Translocator -> NoRestriction
      Realitycollapser -> NoRestriction
      Matterweaver -> exists (PlayableCardWithNoCost NoAction $ InHandOf ForPlay You <> #asset)
      _ -> error "Invalid manifest criteria"

instance RunMessage HyperphysicalShotcasterTheoreticalDevice where
  runMessage msg a@(HyperphysicalShotcasterTheoreticalDevice (With attrs meta)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Todo extend this to full ability
      sid <- getRandom
      when (attrs `hasCustomization` EmpoweredConfiguration)
        $ skillTestModifier sid attrs iid (AnySkillValue 2)
      case manifest meta of
        Just Railshooter -> do
          skillTestModifier sid attrs iid (DamageDealt 1)
          doFight <- Fight.mkChooseFight sid iid attrs
          chooseOne
            iid
            [ SkillLabel sType [toMessage $ Fight.withSkillType sType doFight]
            | sType <- [#willpower, #agility, #intellect, #combat]
            ]
        Just Telescanner -> do
          doInvestigate <- setTarget attrs <$> Investigate.mkInvestigate sid iid attrs
          chooseOne
            iid
            [ SkillLabel sType [toMessage $ Investigate.withSkillType sType doInvestigate]
            | sType <- [#willpower, #agility, #intellect, #combat]
            ]
        Just Translocator -> do
          --  we check if we have enemies we can evade
          canEvade <- select (CanEvadeEnemy $ toSource attrs)

          -- if we don't have an enemy to evade, then we can only move enemies
          -- we can to us, otherwise we can move any non-elite enemy
          canMoveEnemyToUs <-
            select
              $ if notNull canEvade
                then EnemyAt ConnectedLocation <> NonEliteEnemy <> EnemyCanEnter (locationWithInvestigator iid)
                else
                  CanEvadeEnemyWithOverride
                    $ CriteriaOverride
                    $ EnemyCriteria
                    $ ThisEnemy
                    $ EnemyAt ConnectedLocation
                    <> NonEliteEnemy
                    <> EnemyCanEnter (locationWithInvestigator iid)

          -- if we have no enemies to evade, then we can't move any away as we
          -- need to resolve that, if we have exactly one enemy we can evade we
          -- just can't select that one, otherwise we can move any non-elite
          -- enemy
          canMoveEnemyAway <- case canEvade of
            [] -> pure []
            [x] ->
              select
                $ enemyAtLocationWith iid
                <> NonEliteEnemy
                <> EnemyCanEnter (ConnectedFrom $ locationWithInvestigator iid)
                <> not_ (EnemyWithId x)
            _ ->
              select
                $ enemyAtLocationWith iid
                <> NonEliteEnemy
                <> EnemyCanEnter (ConnectedFrom $ locationWithInvestigator iid)

          -- or we move an investigator, as long as we can evade an enemy
          canMoveOtherInvestigatorsAway <-
            if notNull canEvade
              then
                select
                  $ affectsOthers
                  $ colocatedWith iid
                  <> InvestigatorCanMoveTo (toSource attrs) (ConnectedFrom $ locationWithInvestigator iid)
              else pure []

          -- we can move ourself as long as we are engaged with an enemy that can enter the location we can evade OR we will be engaged with an enemy we could evade after moving
          engagedEvadeableEnemy <-
            select
              $ enemyEngagedWith iid
              <> CanEvadeEnemy (toSource attrs)
              <> EnemyCanEnter (ConnectedFrom $ locationWithInvestigator iid)

          locationWeCanMoveToWithCurrentEvade <-
            if notNull engagedEvadeableEnemy
              then
                select
                  $ ConnectedFrom (locationWithInvestigator iid)
                  <> oneOf (map LocationCanBeEnteredBy engagedEvadeableEnemy)
                  <> CanEnterLocation (InvestigatorWithId iid)
              else pure []

          locationWeCanMoveToWithEvadeableEnemies <-
            select
              $ ConnectedFrom (locationWithInvestigator iid)
              <> LocationWithEnemy
                ( CanEvadeEnemyWithOverride
                    $ CriteriaOverride
                    $ EnemyCriteria
                    $ ThisEnemy
                      (oneOf [UnengagedEnemy, MassiveEnemy] <> EnemyAt (ConnectedTo $ locationWithInvestigator iid))
                )

          -- we can do this as long as we have an evade target
          canMoveOtherInvestigatorsToYourLocation <-
            if notNull canEvade
              then
                select
                  $ affectsOthers
                  $ not_ (InvestigatorWithId iid)
                  <> InvestigatorAt (ConnectedFrom $ locationWithInvestigator iid)
                  <> InvestigatorCanMoveTo (toSource attrs) (locationWithInvestigator iid)
              else pure []
          -- We can only choose to move nothing if we have a valid target
          -- If we have a valid target we can move any enemy to us, otherwise we have to move an evadeable enemy
          lid <- getJustLocation iid -- TODO: can we do this?
          chooseOneM iid do
            when (notNull canEvade) $ labeled "Move nothing (before)" $ doStep 1 msg
            targets canMoveEnemyToUs \enemy -> do
              if notNull canEvade
                then enemyMoveTo attrs enemy lid
                else enemyMoveToEdit attrs enemy lid $ afterMove [Msg.EnemyEngageInvestigator enemy iid]
              doStep 0 msg

            targets canMoveEnemyAway \e -> do
              enemyMoveToMatch attrs e (ConnectedFrom $ LocationWithId lid)
              doStep 0 msg

            targets canMoveOtherInvestigatorsAway \i -> do
              moveToMatch attrs i (ConnectedFrom $ LocationWithId lid)
              doStep 0 msg

            targets canMoveOtherInvestigatorsToYourLocation \i -> do
              moveTo attrs i lid
              doStep 0 msg

            targets (locationWeCanMoveToWithCurrentEvade <> locationWeCanMoveToWithEvadeableEnemies) \l -> do
              moveTo attrs iid l
              doStep 0 msg
        Just Realitycollapser -> do
          chooseOne
            iid
            [ SkillLabel sType [Msg.beginSkillTest sid iid (attrs.ability 1) attrs sType (Fixed 3)]
            | sType <- allSkills
            ]
        Just Matterweaver -> do
          cards <- select $ PlayableCardWithNoCost NoAction $ inHandOf ForPlay iid <> #asset
          chooseTargetM iid cards $ handleTarget iid (attrs.ability 1) . toCardId
        _ -> error "Invalid manifest"

      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      card <- getCard cid
      let cost = printedCardCost card
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) (toCardId card) allSkills (Fixed cost)
      pure a
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n < 2 -> do
      sid <- getRandom
      doEvade <- Evade.mkChooseEvade sid iid attrs
      chooseOne
        iid
        [ SkillLabel sType [toMessage $ Evade.withSkillType sType doEvade]
        | sType <- [#willpower, #agility, #intellect, #combat]
        ]
      push $ DoStep 2 msg'
      pure a
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      canMoveEnemyToUs :: [EnemyId] <-
        select $ EnemyAt ConnectedLocation <> NonEliteEnemy <> EnemyCanEnter (locationWithInvestigator iid)

      canMoveEnemyAway <-
        select
          $ enemyAtLocationWith iid
          <> NonEliteEnemy
          <> EnemyCanEnter (ConnectedFrom $ locationWithInvestigator iid)

      canMoveOtherInvestigatorsAway <-
        select
          $ affectsOthers
          $ colocatedWith iid
          <> InvestigatorCanMoveTo (toSource attrs) (ConnectedFrom $ locationWithInvestigator iid)

      locationWeCanMoveTo <-
        select $ CanEnterLocation (InvestigatorWithId iid) <> ConnectedFrom (locationWithInvestigator iid)

      canMoveOtherInvestigatorsToYourLocation <-
        select
          $ affectsOthers
          $ not_ (InvestigatorWithId iid)
          <> InvestigatorAt (ConnectedFrom $ locationWithInvestigator iid)
          <> InvestigatorCanMoveTo (toSource attrs) (locationWithInvestigator iid)
      lid <- getJustLocation iid -- TODO: can we do this?
      chooseOneM iid do
        labeled "Move nothing (after)" nothing
        targets canMoveEnemyToUs \e -> enemyMoveTo attrs e lid
        targets canMoveEnemyAway \e -> enemyMoveToMatch attrs e (ConnectedFrom $ LocationWithId lid)
        targets canMoveOtherInvestigatorsAway \i -> moveToMatch attrs i (ConnectedFrom $ LocationWithId lid)
        targets canMoveOtherInvestigatorsToYourLocation \i -> moveTo attrs i lid
        targets locationWeCanMoveTo $ moveTo attrs iid
      pure a
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      lids <- select $ RevealedLocation <> LocationWithAnyClues
      chooseOrRunOneM iid $ targets lids $ discoverAt IsInvestigate iid attrs 1
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      case manifest meta of
        Just Realitycollapser -> do
          treacheries <- select $ TreacheryIsNonWeakness <> not_ (TreacheryOnEnemy EliteEnemy)
          when (notNull treacheries) $ do
            chooseOne iid [targetLabel t [Msg.toDiscardBy iid (attrs.ability 1) t] | t <- treacheries]
        Just Matterweaver -> do
          mTarget <- getSkillTestTarget
          case mTarget of
            Just (CardIdTarget cid) -> do
              card <- getCard cid
              push $ PutCardIntoPlay iid card Nothing NoPayment (defaultWindows iid)
            _ -> error "Invalid Target"
        _ -> pure ()
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      chooseOne iid
        $ [ Label "Railshooter" [DoStep 0 msg]
          | attrs `hasCustomization` Railshooter
          , manifest meta /= Just Railshooter
          ]
        <> [ Label "Telescanner" [DoStep 1 msg]
           | attrs `hasCustomization` Telescanner
           , manifest meta /= Just Telescanner
           ]
        <> [ Label "Translocator" [DoStep 2 msg]
           | attrs `hasCustomization` Translocator
           , manifest meta /= Just Translocator
           ]
        <> [ Label "Realitycollapser" [DoStep 3 msg]
           | attrs `hasCustomization` Realitycollapser
           , manifest meta /= Just Realitycollapser
           ]
        <> [ Label "Matterweaver" [DoStep 4 msg]
           | attrs `hasCustomization` Matterweaver
           , manifest meta /= Just Matterweaver
           ]

      pure a
    DoStep n (UseThisAbility _iid (isSource attrs -> True) 2) -> do
      let
        manifest'
          | n == 0 = Railshooter
          | n == 1 = Telescanner
          | n == 2 = Translocator
          | n == 3 = Realitycollapser
          | n == 4 = Matterweaver
          | otherwise = error "Invalid manifest"
      pure . HyperphysicalShotcasterTheoreticalDevice . (`with` Metadata (Just manifest')) $ attrs
    _ -> HyperphysicalShotcasterTheoreticalDevice . (`with` meta) <$> liftRunMessage msg attrs
