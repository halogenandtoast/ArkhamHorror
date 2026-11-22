module Arkham.Event.Events.VeilOfThalarion (veilOfThalarion) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Dreaming (getDreamingCards)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher hiding (PlayCard)
import Arkham.Matcher qualified as Matcher
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype VeilOfThalarion = VeilOfThalarion EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

veilOfThalarion :: EventCard VeilOfThalarion
veilOfThalarion = event VeilOfThalarion Cards.veilOfThalarion

{- | "Reduce this card's cost by 1 for each dream on 1 dreaming card you
control." The player names the card, so the reduction is only POTENTIAL here:
'CanReduceCostOf' is what keeps the card offered as playable at its best case,
and the real 'ReduceCostOf' is applied below once the choice is made. The two
halves of the pattern -- see Beach, Fence (1), On Your Own (3).

The best case is the largest dream count, because the reduction is monotone and
no dream is spent.

Read straight off the owner's dream zone: a 'select' or a 'calculate' here would
run on every modifier-collection pass for every card in hand.
-}
instance HasModifiersFor VeilOfThalarion where
  getModifiersFor (VeilOfThalarion a) = do
    dreams <- foldr (max . (.dreams)) 0 <$> getDreamingCards a.owner
    modified_ a a.cardId [CanReduceCostOf (CardWithId a.cardId) dreams | dreams > 0]

{- | The naming happens on the play window, which is where the cost is still
open: the active cost is built from 'getCostForCard' only once @InitiatePlayCard@
runs, after @PlayCard #when@.

Unprinted as an ability -- the card states the reduction as a flat line -- so it
is 'SilentForcedAbility': no "do you want to use this?", just the choice the
printed text calls for.
-}
instance HasAbilities VeilOfThalarion where
  getAbilities (VeilOfThalarion a) =
    [ restricted a 1 (DreamingCardExists dreamToSpend)
        $ SilentForcedAbility
        $ Matcher.PlayCard #when You (basic $ CardWithId a.cardId)
    ]
   where
    dreamToSpend = DreamingCardControlledBy You <> DreamingCardWithDreams (atLeast 1)

{- | All three would-take windows are raised together (Investigator/Runner/Damage.hs),
so the combined one carries both numbers; the singles are only a fallback.
-}
getDamageAndHorror :: [Window] -> Maybe (InvestigatorId, Int, Int)
getDamageAndHorror ws = foldr merge Nothing (mapMaybe toPayload ws)
 where
  toPayload w = case windowType w of
    Window.WouldTakeDamageOrHorror _ (InvestigatorTarget iid) damage horror -> Just (iid, damage, horror)
    Window.WouldTakeDamage _ (InvestigatorTarget iid) damage _ -> Just (iid, damage, 0)
    Window.WouldTakeHorror _ (InvestigatorTarget iid) horror -> Just (iid, 0, horror)
    _ -> Nothing
  merge x Nothing = Just x
  merge (iid, damage, horror) (Just (_, damage', horror')) =
    Just (iid, max damage damage', max horror horror')

instance RunMessage VeilOfThalarion where
  runMessage msg e@(VeilOfThalarion attrs) = runQueueT $ case msg of
    {- "1 dreaming card you control": the player names it. Asked only when the
    answer can differ -- when every candidate holds the same number of dreams the
    reduction is identical whichever is named, so the prompt is pure noise. Best
    first, since a smaller reduction is never an advantage.

    A card holding no dreams is not a candidate: it reduces nothing, and at 0
    dreams it should already have flipped faceup (a card stranded there is the
    uniqueness case). Offering it would also split the counts and turn a forced
    case into a prompt. -}
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      dreamingCards <-
        sortOn (Down . (.dreams)) . filter ((> 0) . (.dreams)) <$> getDreamingCards attrs.owner
      case nub (map (.dreams) dreamingCards) of
        [] -> pure ()
        [dreams] -> reduceCostOf (attrs.ability 1) (toCard attrs) dreams
        _ ->
          chooseOneM iid $ targets dreamingCards \dreamingCard ->
            reduceCostOf (attrs.ability 1) (toCard attrs) dreamingCard.dreams
      pure e
    InvestigatorPlayEvent _ (is attrs -> True) _ (getDamageAndHorror -> Just (iid, damage, horror)) _ -> do
      pushWhen (damage > 0) $ CancelDamage iid damage
      pushWhen (horror > 0) $ CancelHorror iid horror
      cancelledOrIgnoredCardOrGameEffect attrs
      pure e
    _ -> VeilOfThalarion <$> liftRunMessage msg attrs
