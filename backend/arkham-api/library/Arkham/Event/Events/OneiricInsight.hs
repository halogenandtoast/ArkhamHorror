module Arkham.Event.Events.OneiricInsight (oneiricInsight) where

import Arkham.Card
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), withModifiersOf)
import Arkham.Helpers.Playable (getIsPlayable)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Window (mkWhen)
import Arkham.Window qualified as Window

newtype OneiricInsight = OneiricInsight EventAttrs
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneiricInsight :: EventCard OneiricInsight
oneiricInsight = event OneiricInsight Cards.oneiricInsight

instance RunMessage OneiricInsight where
  runMessage msg e@(OneiricInsight attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      investigators <- select $ affectsOthers Anyone
      chooseOrRunOneM iid $ targets investigators \iid' ->
        search
          iid'
          attrs
          iid'
          [fromTopOfDeck 9]
          (basic $ #asset <> NonWeakness <> CardWithLevel 0)
          (defer attrs IsNotDraw)
      pure e
    SearchNoneFound iid (isTarget attrs -> True) -> do
      shuffleDeck iid
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      -- the windows the search itself would have played the asset in
      let ws = [mkWhen Window.NonFast, mkWhen (Window.DuringTurn iid)]
      {- "as if it had dreaming X, where X is that asset's printed resource
      cost": an asset with no printed resource cost has no X, so a dreaming card
      found here is not a legal choice. The grant has to be live while
      playability is tested or every candidate reads as unplayable -- a dreaming
      card has no resource cost to pay. -}
      options <- forMaybeM cards \card -> case (toCardDef card).cost of
        Just (StaticCost x) -> do
          playable <-
            withModifiersOf card attrs [AddKeyword (Keyword.Dreaming x)]
              $ getIsPlayable iid attrs (UnpaidCost NoAction) ws card
          pure $ guard playable $> (card, x)
        _ -> pure Nothing
      when (notNull options) do
        chooseOneM iid $ for_ options \(card, x) -> targeting card do
          {- Exactly the play this card paid for, and no longer. The grant has to
          span hand -> facedown play -> the multi-turn dream -> the flip, so an
          `EffectCostWindow` grant (what `costModifier` makes) is too short: it
          expires with cost payment and the dreamed card is left with no dreaming
          value at all.

          'EffectCardResolutionWindow' ends on @ResolvedCard@ for this card id,
          which cannot arrive early: the only pushers are the `PutCardIntoPlay`
          branches and the draw/revelation paths, and the facedown play never
          reaches `PutCardIntoPlay` (`PlayCard` short-circuits on a dreamed
          payment). At the flip it is pushed LAST by the asset branch, after that
          handler has already read `getDreamingValue` to tell the enters-play
          apart from a play -- so the grant covers that read and dies immediately
          after it. -}
          cardResolutionModifier card attrs (CardIdTarget card.id) (AddKeyword (Keyword.Dreaming x))
          playCardPayingCostWithWindows iid card ws
      shuffleDeck iid
      pure e
    _ -> OneiricInsight <$> liftRunMessage msg attrs
