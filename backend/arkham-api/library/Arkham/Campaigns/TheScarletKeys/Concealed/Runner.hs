{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Campaigns.TheScarletKeys.Concealed.Runner where

import Arkham.Calculation
import Arkham.Campaigns.TheScarletKeys.Concealed
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Classes.RunMessage
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.SkillTest.Lifted (beginSkillTestEdit)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Placement
import Arkham.Prelude
import Arkham.SkillTest.Base
import Arkham.Source
import Arkham.Target

instance RunMessage ConcealedCard where
  runMessage msg c = runQueueT $ case msg of
    PlaceConcealedCard _iid cardId placement | c.id == cardId -> do
      pure $ c {concealedCardPlacement = placement}
    UseThisAbility iid (isSource c -> True) 1 -> do
      case c.placement of
        AtLocation location -> do
          sid <- getRandom
          beginSkillTestEdit
            sid
            iid
            (c.ability 1)
            c
            #combat
            (LocationMaybeFieldCalculation location LocationShroud)
            \st ->
              st {skillTestAction = Just #fight}
        _ -> pure ()
      pure c
    UseThisAbility iid (isSource c -> True) 2 -> do
      case c.placement of
        AtLocation location -> do
          sid <- getRandom
          beginSkillTestEdit
            sid
            iid
            (c.ability 2)
            c
            #agility
            (LocationMaybeFieldCalculation location LocationShroud)
            \st ->
              st {skillTestAction = Just #evade}
        _ -> pure ()
      pure c
    PassedThisSkillTest iid (isAbilitySource c 1 -> True) -> do
      push $ Flip iid (c.ability 1) (toTarget c)
      pure c
    PassedThisSkillTest iid (isAbilitySource c 2 -> True) -> do
      push $ Flip iid (c.ability 2) (toTarget c)
      pure c
    Flip iid _ (isTarget c -> True) -> do
      chooseTargetM iid [c] \_ -> doStep 1 msg
      pure $ c {concealedCardFlipped = True}
    DoStep 1 msg'@(Flip _iid _ (isTarget c -> True)) -> do
      let getConcealedEnemy card = whenJustM (selectOne (EnemyWithPlacement InTheShadows <> enemyIs card))
      case c.placement of
        AtLocation location -> do
          case c.kind of
            Decoy -> pure ()
            TheRedGlovedMan -> getConcealedEnemy Enemies.theRedGlovedManShroudedInMystery \enemy ->
              enemyMoveTo c enemy location
            _ -> error "Unhandled"
        _ -> pure ()
      doStep 2 msg'
      pure $ c {concealedCardPlacement = Unplaced}
    DoStep 2 (Flip _iid _ (isTarget c -> True)) -> do
      removeFromGame (toTarget c)
      inShadows <- selectAny (EnemyWithPlacement InTheShadows)
      unless inShadows $ push RemoveAllConcealed
      pure c
    RemoveAllConcealed -> do
      removeFromGame (toTarget c)
      pure c
    UseAbility _ ab _ | isSource c ab.source || isProxySource c ab.source -> do
      do_ msg
      pure c
    _ -> pure c
