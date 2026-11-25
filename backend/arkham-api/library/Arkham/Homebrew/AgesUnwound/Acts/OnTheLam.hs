module Arkham.Homebrew.AgesUnwound.Acts.OnTheLam (onTheLam) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card (genCard)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.ScenarioDeckKeys (pattern ArkhamStreetsDeck)
import Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers
import Arkham.Matcher

newtype OnTheLam = OnTheLam ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheLam :: ActCard OnTheLam
onTheLam = act (1, A) OnTheLam Cards.onTheLam Nothing

{- | "(You may initiate a move even if there are no connecting locations in play.)" --
the permission the reaction below carries, which is not an ability of its own. With
it, an isolated location is accessible to itself, so the location's ordinary move
action and every card that offers a move to a connecting location can create the
move that ability 1 then redirects. Pointless with an empty deck, where the redirect
could only cancel the move it was offered for.
-}
instance HasModifiersFor OnTheLam where
  getModifiersFor (OnTheLam a) =
    whenM (notNull <$> getArkhamStreetsDeck)
      $ modifySelect a Anywhere [ConnectsToSelfWhenIsolated]

-- NB. index 2 was a stand-in move action for the permission now granted above; left
-- unused so a game parked on a question that offered it cannot resolve something else.
instance HasAbilities OnTheLam where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1 $ triggered redirectableMove Free
    , restricted a 3 (exists EmptyLocation) $ forced $ RoundEnds #when
    ]

instance RunMessage OnTheLam where
  runMessage msg a@(OnTheLam attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      redirectToNewArkhamStreetsLocation (attrs.ability 1) iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      cards <- selectForM EmptyLocation \lid -> do
        removeLocationWithoutVictory lid
        fetchCard lid
      shuffleCardsIntoDeck ArkhamStreetsDeck cards
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push . ReplaceAct attrs.id =<< genCard Cards.onTheLam
      pure a
    _ -> OnTheLam <$> liftRunMessage msg attrs
