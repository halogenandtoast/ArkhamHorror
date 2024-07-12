module Arkham.Event.Cards.MakeshiftTrap (makeshiftTrap, MakeshiftTrap (..)) where

import Arkham.Ability
import Arkham.DamageEffect
import Arkham.Enemy.Types (Field (EnemyPlacement))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Customization
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Helpers.Location (onSameLocation)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Helpers.Use (toModifiedStartingUses)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Token
import Arkham.Window qualified as Window
import Data.Map.Strict qualified as Map

newtype MakeshiftTrap = MakeshiftTrap EventAttrs
  deriving anyclass (IsEvent)
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
  getModifiersFor (EnemyTarget eid) (MakeshiftTrap attrs) = do
    placement <- field EnemyPlacement eid
    sameLocation <- onSameLocation placement attrs.placement
    let standard = guard sameLocation *> [EnemyFight (-1), EnemyEvade (-1)]
    net <-
      fromMaybe [] <$> runMaybeT do
        guard $ attrs `hasCustomization` Net
        guard sameLocation
        liftGuardM $ eid <=~> NonEliteEnemy
        pure [CannotMakeAttacksOfOpportunity, CannotMove]

    modified attrs $ standard <> net
  getModifiersFor target (MakeshiftTrap a) | isTarget a target = do
    modified a $ guard (a `hasCustomization` Simple) $> BecomesFast FastPlayerWindow
  getModifiersFor _ _ = pure []

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
              [targetLabel lid' [PlaceEvent iid attrs.id (AttachedToLocation lid')] | lid' <- lid : connected]
          else push $ PlaceEvent iid attrs.id (AttachedToLocation lid)
      pure e
    MoveTokens s source _ tType n | isSource attrs source -> runMessage (RemoveTokens s (toTarget attrs) tType n) e
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
      runMessage (RemoveTokens source target useType' n) e
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ RemoveTokens (attrs.ability 1) (toTarget attrs) Time 1
      pure e
    _ -> MakeshiftTrap <$> liftRunMessage msg attrs
