module Arkham.Homebrew.AgainstTheWendigo.Agendas.TheWendigoHuntsYou (theWendigoHuntsYou) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Enemies
import Arkham.Matcher
import Arkham.Trait (Trait (Ally))
import Arkham.Resolution

newtype TheWendigoHuntsYou = TheWendigoHuntsYou AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWendigoHuntsYou :: AgendaCard TheWendigoHuntsYou
theWendigoHuntsYou = agenda (3, A) TheWendigoHuntsYou Cards.theWendigoHuntsYou (Static 8)

instance HasAbilities TheWendigoHuntsYou where
  getAbilities (TheWendigoHuntsYou a) =
    [ {- | "Forced - When at least 1 damage is placed on an investigator or an
      Ally asset: Add 1 doom to this agenda. (Collective limit of 1 doom per
      round added by this effect.)" -}
      groupLimit PerRound
        $ restricted a 1 (not_ $ AgendaExists $ AgendaWithSide B)
        $ forced
        $ oneOf
          [ DealtDamage #after AnySource Anyone
          , AssetDealtDamage #after AnySource (AssetWithTrait Ally)
          ]
    , -- Agenda 3b, The Wendigo's Attack: "Objective - If The Wendigo is
      -- defeated: (-> R1)."
      restricted a 2 (notExists $ enemyIs Enemies.theWendigo) $ Objective freeTrigger_
    ]

instance RunMessage TheWendigoHuntsYou where
  runMessage msg a@(TheWendigoHuntsYou attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ ScenarioResolution $ Resolution 1
      pure a
    -- "The Wendigo's Attack - This card replaces the current agenda. Put The
    -- Wendigo into play."
    AdvanceAgenda (isSide B attrs -> True) -> do
      createEnemyAtLocationMatching_ Enemies.theWendigo (connectedFrom (LocationWithInvestigator MostDamage))
      pure a
    _ -> TheWendigoHuntsYou <$> liftRunMessage msg attrs
