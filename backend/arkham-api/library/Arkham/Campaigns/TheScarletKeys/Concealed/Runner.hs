{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Campaigns.TheScarletKeys.Concealed.Runner where

import Arkham.Action qualified as Action
import Arkham.Calculation
import Arkham.Campaigns.TheScarletKeys.Concealed
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Card.CardDef
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Classes.RunMessage
import Arkham.Constants
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Fight.Types
import Arkham.Helpers.SkillTest.Lifted (beginSkillTestEdit, evade, fight)
import Arkham.Id
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted hiding (choose)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Placement
import Arkham.Prelude
import Arkham.SkillTest.Base
import Arkham.Source
import Arkham.Target

isEnemyTarget :: ConcealedCard -> Target -> Bool
isEnemyTarget c target =
  isTarget (EnemyId $ coerce $ unConcealedCardId c.id) target || isActionTarget c target
 where
  isActionTarget a = isTarget a . toProxyTarget

concealedToCardDef :: ConcealedCard -> Maybe CardDef
concealedToCardDef c = case c.kind of
  Decoy -> Nothing
  AcolyteAny -> Just Enemies.acolyte
  ApportionedKa -> Just Enemies.apportionedKa
  CoterieAgentA -> Just Enemies.coterieAgentA
  CoterieAgentB -> Just Enemies.coterieAgentB
  CoterieAgentC -> Just Enemies.coterieAgentC
  EmissaryFromYuggoth -> Just Enemies.emissaryFromYuggoth
  LaChicaRoja -> Just Enemies.laChicaRojaTheGirlInTheCarmineCoat
  SinisterAspirantA -> Just Enemies.sinisterAspirantA
  SinisterAspirantB -> Just Enemies.sinisterAspirantB
  SinisterAspirantC -> Just Enemies.sinisterAspirantC
  TheRedGlovedMan -> Just Enemies.theRedGlovedManShroudedInMystery
  WizardOfTheOrder -> Just Enemies.wizardOfTheOrder
  _ -> error "Unhandled Concealed Card Kind"

instance RunMessage ConcealedCard where
  runMessage msg c = runQueueT $ case msg of
    PlaceConcealedCard _iid cardId placement | c.id == cardId -> do
      pure
        $ c
          { concealedCardPlacement = placement
          , concealedCardKnown = False
          , concealedCardFlipped = False
          }
    UseThisAbility iid (isSource c -> True) AbilityAttack -> do
      case c.placement of
        AtLocation location -> do
          sid <- getRandom
          let difficulty = LocationMaybeFieldCalculation location LocationShroud
          beginSkillTestEdit sid iid (c.ability AbilityAttack) c #combat difficulty \st ->
            st {skillTestAction = Just #fight}
        _ -> pure ()
      pure c
    PassedThisSkillTest iid (isAbilitySource c AbilityAttack -> True) -> do
      case c.placement of
        AtLocation location -> do
          whenM (matches location $ LocationWithoutModifier (CampaignModifier "noExposeAt")) do
            push $ Flip iid (c.ability AbilityAttack) (toTarget c)
        _ -> push $ Flip iid (c.ability AbilityAttack) (toTarget c)
      pure c
    UseThisAbility iid (isSource c -> True) AbilityEvade -> do
      case c.placement of
        AtLocation location -> do
          sid <- getRandom
          let difficulty = LocationMaybeFieldCalculation location LocationShroud
          beginSkillTestEdit sid iid (c.ability AbilityEvade) c #agility difficulty \st ->
            st {skillTestAction = Just #evade}
        _ -> pure ()
      pure c
    PassedThisSkillTest iid (isAbilitySource c AbilityEvade -> True) -> do
      case c.placement of
        AtLocation location -> do
          whenM (matches location $ LocationWithoutModifier (CampaignModifier "noExposeAt")) do
            push $ Flip iid (c.ability AbilityEvade) (toTarget c)
        _ -> push $ Flip iid (c.ability AbilityEvade) (toTarget c)
      pure c
    Flip iid _ (isTarget c -> True) -> do
      chooseTargetM iid [c] \_ -> doStep 1 msg
      pure $ c {concealedCardFlipped = True, concealedCardKnown = True}
    DoStep 1 msg'@(Flip iid _ (isTarget c -> True)) -> do
      case concealedToCardDef c of
        Nothing -> case c.kind of
          Decoy -> do
            exposedDecoy iid
            removeFromGame (toTarget c)
          _ -> pure ()
        Just def -> do
          enemies <- select $ EnemyWithPlacement InTheShadows <> EnemyWithTitle def.title
          chooseOrRunOneM iid do
            targets enemies \enemy -> do
              exposed iid enemy def do
                case c.placement of
                  AtLocation location -> enemyMoveToIfInPlay c enemy location
                  _ -> error "invalid placement for concealed card"
                doStep 2 msg'
      pure $ c {concealedCardPlacement = Unplaced}
    DoStep 2 (Flip _iid _ (isTarget c -> True)) -> do
      removeFromGame (toTarget c)
      inShadows <- selectAny (EnemyWithPlacement InTheShadows)
      unless inShadows $ push RemoveAllConcealed
      pure c
    AttackEnemy eid choose | eid == coerce (unConcealedCardId c.id) -> do
      let iid = choose.investigator
      let source = choose.source
      let sid = choose.skillTest
      let target = maybe (toTarget c) (ProxyTarget (toTarget c)) choose.target
      let skillType = choose.skillType
      let
        difficulty =
          case choose.difficulty of
            DefaultChooseFightDifficulty -> case c.placement of
              AtLocation location -> LocationMaybeFieldCalculation location LocationShroud
              _ -> error "invalid placement for concealed card"
            CalculatedChooseFightDifficulty ccfd -> ccfd

      fight sid iid source target skillType difficulty
      pure c
    PassedSkillTest iid (Just Action.Fight) source (Initiator target) _ _ | isEnemyTarget c target -> do
      push $ Flip iid source (toTarget c)
      pure c
    TryEvadeEnemy sid iid eid source mTarget skillType | eid == coerce (unConcealedCardId c.id) -> do
      case c.placement of
        AtLocation location -> do
          let target = maybe (toTarget eid) (ProxyTarget (toTarget eid)) mTarget
          let difficulty = LocationMaybeFieldCalculation location LocationShroud
          evade sid iid source target skillType difficulty
        _ -> error "invalid placement for concealed card"
      pure c
    PassedSkillTest iid (Just Action.Evade) source (Initiator target) _ _ | isEnemyTarget c target -> do
      push $ Flip iid source (toTarget c)
      pure c
    RemoveAllConcealed -> do
      removeFromGame (toTarget c)
      pure c
    LookAtRevealed iid _ (isTarget c -> True) -> do
      chooseOneM iid $ targeting c $ doStep 1 msg
      pure $ c {concealedCardKnown = True, concealedCardFlipped = True}
    DoStep 1 (LookAtRevealed _ _ (isTarget c -> True)) -> do
      pure $ c {concealedCardFlipped = False}
    DoStep 2 (LookAtRevealed _ _ (isTarget c -> True)) -> do
      pure $ c {concealedCardKnown = True, concealedCardFlipped = True}
    PlaceConcealedCard _ card placement | card /= c.id && c.placement == placement -> do
      pure $ c {concealedCardKnown = False}
    UseAbility _ ab _ | isSource c ab.source || isProxySource c ab.source -> do
      do_ msg
      pure c
    SetLocationOutOfGame lid -> do
      case c.placement of
        p@(AtLocation lid') | lid' == lid -> pure $ c {concealedCardPlacement = OutOfGame p}
        p@(AttachedToLocation lid') | lid' == lid -> pure $ c {concealedCardPlacement = OutOfGame p}
        _ -> pure c
    ReturnLocationToGame lid -> do
      case c.placement of
        OutOfGame p@(AtLocation lid') | lid' == lid -> pure $ c {concealedCardPlacement = p}
        OutOfGame p@(AttachedToLocation lid') | lid' == lid -> pure $ c {concealedCardPlacement = p}
        _ -> pure c
    _ -> pure c
