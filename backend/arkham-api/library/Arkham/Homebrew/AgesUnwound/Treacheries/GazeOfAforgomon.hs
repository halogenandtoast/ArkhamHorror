module Arkham.Homebrew.AgesUnwound.Treacheries.GazeOfAforgomon (gazeOfAforgomon) where

import Arkham.Ability
import Arkham.Criteria qualified as Criteria
import Arkham.GameEnv (getHistory)
import Arkham.History (History (historyActionsCompleted))
import Arkham.History.Types
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Token qualified as Token
import Arkham.Treachery.Import.Lifted

newtype GazeOfAforgomon = GazeOfAforgomon TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gazeOfAforgomon :: TreacheryCard GazeOfAforgomon
gazeOfAforgomon = treachery GazeOfAforgomon Cards.gazeOfAforgomon

instance HasAbilities GazeOfAforgomon where
  getAbilities (GazeOfAforgomon a) =
    [ -- "Forced - At the end of your turn, if you have taken at least 4 actions
      -- this turn: Place 1 resource on Gaze of Aforgomon, as an offering."
      restricted a 1 InYourThreatArea $ forced $ TurnEnds #when You
    , -- "[action][action]: Place 1 resource on Gaze of Aforgomon, as an offering."
      restricted a 2 InYourThreatArea $ ActionAbility mempty Nothing (ActionCost 2)
    , -- "Forced - At the end of the game, if Gaze of Aforgomon has fewer than 4
      -- offerings on it: You suffer 1 physical trauma."
      restricted a 3 (InYourThreatArea <> Criteria.ResourcesOnThis (lessThan 4))
        $ forced
        $ GameEnds #when
    ]

instance RunMessage GazeOfAforgomon where
  runMessage msg t@(GazeOfAforgomon attrs) = runQueueT $ case msg of
    -- "Revelation - Put Gaze of Aforgomon into play in your threat area."
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      actions <- historyActionsCompleted <$> getHistory TurnHistory iid
      when (actions >= 4) $ placeTokens (attrs.ability 1) attrs Token.Resource 1
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      placeTokens (attrs.ability 2) attrs Token.Resource 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      sufferPhysicalTrauma iid 1
      pure t
    _ -> GazeOfAforgomon <$> liftRunMessage msg attrs
