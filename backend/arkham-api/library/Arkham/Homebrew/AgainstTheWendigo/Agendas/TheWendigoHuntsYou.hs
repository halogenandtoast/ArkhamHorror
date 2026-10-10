{- | Agenda 3a, The Wendigo Hunts You.

Its back, The Wendigo's Attack, replaces the current agenda and then stays, so
it is an agenda of its own here -- advancing this one puts The Wendigo into play
and hands over to it. See
"Arkham.Homebrew.AgainstTheWendigo.Agendas.TheWendigosAttack".
-}
module Arkham.Homebrew.AgainstTheWendigo.Agendas.TheWendigoHuntsYou (theWendigoHuntsYou) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Enemies
import Arkham.Matcher
import Arkham.Trait (Trait (Ally))

newtype TheWendigoHuntsYou = TheWendigoHuntsYou AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWendigoHuntsYou :: AgendaCard TheWendigoHuntsYou
theWendigoHuntsYou = agenda (3, A) TheWendigoHuntsYou Cards.theWendigoHuntsYou (Static 8)

{- | "Forced - When at least 1 damage is placed on an investigator or an Ally
asset: Add 1 doom to this agenda. (Collective limit of 1 doom per round added by
this effect. This effect may advance the current agenda.)"

The Civilized locations' "{action}: Resign" is granted from agenda 2 onwards and
is read off the agenda step by the locations themselves; see 'civilizedResign'.
-}
instance HasAbilities TheWendigoHuntsYou where
  getAbilities (TheWendigoHuntsYou a) =
    [ groupLimit PerRound
        $ mkAbility a 1
        $ forced
        $ oneOf
          [ DealtDamage #after AnySource Anyone
          , AssetDealtDamage #after AnySource (AssetWithTrait Ally)
          ]
    ]

instance RunMessage TheWendigoHuntsYou where
  runMessage msg a@(TheWendigoHuntsYou attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      -- "This effect may advance the current agenda."
      push AdvanceAgendaIfThresholdSatisfied
      pure a
    {- The back: "Revelation - This card replaces the current agenda. Put The
    Wendigo into play." The deck is already mid-advance and its windows have been
    checked, so this hands over without checking them a second time. -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      createEnemyAtLocationMatching_
        Enemies.theWendigo
        (connectedFrom (LocationWithInvestigator MostDamage))
      {- Discarding an act empties the act stack, so nothing follows it; The
      Wendigo's Attack carries the only objective left. -}
      selectEach AnyAct $ toDiscard attrs
      push $ Do (AdvanceToAgenda attrs.deck Cards.theWendigosAttack Agenda.A (toSource attrs))
      pure a
    _ -> TheWendigoHuntsYou <$> liftRunMessage msg attrs
