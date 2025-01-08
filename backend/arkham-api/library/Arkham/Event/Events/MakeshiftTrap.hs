module Arkham.Event.Events.MakeshiftTrap (makeshiftTrap, MakeshiftTrap (..)) where

import Arkham.Ability
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Customization
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMapM, modifySelfWhen)
import Arkham.Helpers.Use (toModifiedStartingUses)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Token
import Arkham.Window qualified as Window
import Data.Map.Strict qualified as Map

newtype MakeshiftTrap = MakeshiftTrap EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

makeshiftTrap :: EventCard MakeshiftTrap
makeshiftTrap = event MakeshiftTrap Cards.makeshiftTrap

instance HasAbilities MakeshiftTrap where
  getAbilities (MakeshiftTrap a) = [restrictedAbility a 1 tripwireCriteria $ forced $ RoundEnds #when]
   where
    tripwireCriteria = case a.attachedTo of
      Just (LocationTarget lid) ->
        if a `hasCustomization` Tripwire
          then ControlsThis <> exists (EnemyAt $ LocationWithId lid)
          else ControlsThis
      _ -> ControlsThis

instance HasModifiersFor MakeshiftTrap where
  getModifiersFor (MakeshiftTrap a) = do
    enemies <- case a.placement of
      AttachedToLocation lid -> modifySelectMapM a (EnemyAt $ LocationWithId lid) \eid -> do
        net <-
          fromMaybe [] <$> runMaybeT do
            guard $ a `hasCustomization` Net
            liftGuardM $ eid <=~> NonEliteEnemy
            pure [CannotMakeAttacksOfOpportunity, CannotMove]

        pure $ [EnemyFight (-1), EnemyEvade (-1)] <> net
      _ -> pure mempty
    self <- modifySelfWhen a (a `hasCustomization` Simple) [BecomesFast FastPlayerWindow]
    pure $ self <> enemies

-- We need to ensure all messages that run RemoveTokens directly are captured and handled here
instance RunMessage MakeshiftTrap where
  runMessage msg e@(MakeshiftTrap attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      n <-
        Map.findWithDefault 0 Time <$> toModifiedStartingUses attrs (eventPrintedUses attrs)
      if attrs `hasCustomization` ImprovedTimer
        then
          chooseOne
            iid
            [ Label ("Place " <> tshow x <> " time") [PlaceTokens (toSource attrs) (toTarget attrs) Time x]
            | x <- [n - 1, n, n + 1]
            ]
        else push $ PlaceTokens (toSource attrs) (toTarget attrs) Time n
      withLocationOf iid \lid -> do
        if attrs `hasCustomization` RemoteConfiguration
          then do
            connected <- select $ ConnectedFrom (LocationWithId lid) <> RevealedLocation
            chooseOrRunOne
              iid
              [targetLabel lid' [PlaceEvent attrs.id (AttachedToLocation lid')] | lid' <- lid : connected]
          else push $ PlaceEvent attrs.id (AttachedToLocation lid)
      pure e
    MoveTokens s source _ tType n | isSource attrs source -> do
      liftRunMessage (RemoveTokens s (toTarget attrs) tType n) e
    RemoveTokens _ target tType n | isTarget attrs target -> do
      let m = findWithDefault 0 tType attrs.tokens
      let remainingUses = max 0 (m - n)
      when (n > 0 && attrs `hasCustomization` Poisonous) do
        for_ attrs.attachedTo \case
          LocationTarget lid -> do
            selectWithNonNull (EnemyAt (LocationWithId lid) <> EnemyCanBeDamagedBySource (toSource attrs)) \enemies ->
              chooseOne
                attrs.controller
                [targetLabel enemy [EnemyDamage enemy $ nonAttack attrs 1] | enemy <- enemies]
          _ -> pure ()
      when (remainingUses == 0) $ do
        when (attrs `hasCustomization` ExplosiveDevice) do
          for_ attrs.attachedTo \case
            LocationTarget lid -> do
              enemies <- select $ EnemyAt (LocationWithId lid) <> EnemyCanBeDamagedBySource (toSource attrs)
              investigators <- select $ InvestigatorAt (LocationWithId lid)
              when (notNull enemies || notNull investigators) do
                uiEffect attrs lid Explosion
                chooseOneAtATime attrs.controller
                  $ [targetLabel enemy [EnemyDamage enemy $ nonAttack attrs 3] | enemy <- enemies]
                  <> [targetLabel investigator [Msg.assignDamage investigator attrs 3] | investigator <- investigators]
            _ -> pure ()

        toDiscardBy attrs.controller GameSource attrs
      MakeshiftTrap <$> liftRunMessage msg attrs
    Do (SpendUses source target useType' n) | isTarget attrs target -> do
      checkAfter $ Window.SpentToken source (toTarget attrs) useType' n
      liftRunMessage (RemoveTokens source target useType' n) e
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ RemoveTokens (attrs.ability 1) (toTarget attrs) Time 1
      pure e
    _ -> MakeshiftTrap <$> liftRunMessage msg attrs
