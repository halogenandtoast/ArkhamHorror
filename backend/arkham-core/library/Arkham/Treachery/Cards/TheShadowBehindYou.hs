module Arkham.Treachery.Cards.TheShadowBehindYou
  ( theShadowBehindYou
  , TheShadowBehindYou(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Attrs
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Attrs
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Metadata = Metadata { usedAbility :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheShadowBehindYou = TheShadowBehindYou (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theShadowBehindYou :: TreacheryCard TheShadowBehindYou
theShadowBehindYou = treachery
  (TheShadowBehindYou . (`with` Metadata False))
  Cards.theShadowBehindYou

instance HasAbilities TheShadowBehindYou where
  getAbilities (TheShadowBehindYou (a `With` metadata)) =
    restrictedAbility a 1 OnSameLocation (ActionAbility Nothing $ ActionCost 1)
      : [ restrictedAbility
            a
            2
            (InThreatAreaOf You <> InvestigatorExists
              (You <> AnyInvestigator
                [ InvestigatorWithAnyResources
                , HandWith (HasCard $ CardWithoutKeyword Hidden)
                ]
              )
            )
          $ ForcedAbility
          $ TurnEnds Timing.When You
        | not (usedAbility metadata)
        ]

instance TreacheryRunner env => RunMessage env TheShadowBehindYou where
  runMessage msg t@(TheShadowBehindYou (attrs `With` metadata)) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      pure $ TheShadowBehindYou (attrs `with` Metadata True)
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      -- hidden cards can cause the then effect to fail
      hasResources <- fieldP InvestigatorResources (> 0) iid
      handCards <- field InvestigatorHand iid
      let
        hasNonHiddenCards = any (notElem Hidden . toKeywords) handCards
        hasHiddenCards = any (elem Hidden . toKeywords) handCards

      push
        $ chooseOrRunOne iid
        $ [ Label
              "Discard all cards in your hand"
              (DiscardHand iid
              : [ Discard (toTarget attrs) | not hasHiddenCards ]
              )
          | hasNonHiddenCards
          ]
        <> [ Label
               "Lose all resources"
               [LoseAllResources iid, Discard (toTarget attrs)]
           | hasResources
           ]
      pure t
    EndTurn iid | treacheryOnInvestigator iid attrs ->
      pure $ TheShadowBehindYou (attrs `with` Metadata False)
    _ -> TheShadowBehindYou . (`with` metadata) <$> runMessage msg attrs
