module Arkham.Enemy.Cards.TheInnsmouthConspiracy.IntoTheMaelstrom.HydraAwakenedAndEnraged (hydraAwakenedAndEnraged) where

import Arkham.Ability
import Arkham.DamageEffect
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Sanctum))
import Arkham.UltimatumsAndBoons (hasUltimatum)
import Arkham.UltimatumsAndBoons.Types (
  Ultimatum (UltimatumOfTheDrowned),
  UltimatumOrBoon (Ultimatum),
 )
import Arkham.Window qualified as Window

newtype HydraAwakenedAndEnraged = HydraAwakenedAndEnraged EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hydraAwakenedAndEnraged :: EnemyCard HydraAwakenedAndEnraged
hydraAwakenedAndEnraged =
  enemyWith
    HydraAwakenedAndEnraged
    Cards.hydraAwakenedAndEnraged
    (healthL .~ Nothing)

instance HasModifiersFor HydraAwakenedAndEnraged where
  getModifiersFor (HydraAwakenedAndEnraged a) = do
    n <- selectCount $ LocationWithAnyKeys <> withTrait Sanctum
    modifySelfWhen a (n > 0) [EnemyFight (-n)]

instance HasAbilities HydraAwakenedAndEnraged where
  getAbilities (HydraAwakenedAndEnraged a) =
    extend1 a
      {- The Ultimatum of the Drowned's rewritten Forced readies Hydra even with no
      Brood in play, so the criterion has to admit that case too. -}
      $ restricted
        a
        1
        ( oneOf
            [ exists $ enemyIs Cards.hydrasBrood
            , UltimatumOrBoonIsActive (Ultimatum UltimatumOfTheDrowned)
            ]
        )
      $ forced
      $ oneOf [EnemyDealtDamage #after AnyDamageEffect (be a) AnySource, EnemyEvaded #after Anyone (be a)]

instance RunMessage HydraAwakenedAndEnraged where
  runMessage msg e@(HydraAwakenedAndEnraged attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (map Window.windowType -> ws) _ -> do
      brood <- select $ enemyIs Cards.hydrasBrood
      for_ ws \case
        Window.EnemyEvaded eiid _ ->
          chooseOrRunTargetM iid brood $ push . Msg.EnemyEvaded eiid
        Window.DealtDamage source damageEffect _ n ->
          chooseOrRunTargetM iid brood \target ->
            push $ DealDamage (EnemyTarget target) $ DamageAssignment source n damageEffect False False
        _ -> pure ()

      -- The Ultimatum of the Drowned adds "Ready Hydra." to this Forced.
      whenM (hasUltimatum UltimatumOfTheDrowned) $ readyThis attrs

      pure e
    _ -> HydraAwakenedAndEnraged <$> liftRunMessage msg attrs
