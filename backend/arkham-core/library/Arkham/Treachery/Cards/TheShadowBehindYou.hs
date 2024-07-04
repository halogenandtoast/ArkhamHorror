module Arkham.Treachery.Cards.TheShadowBehindYou (theShadowBehindYou, TheShadowBehindYou (..)) where

import Arkham.Ability
import Arkham.Helpers.Message qualified as Msg
import Arkham.Investigator.Types
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Metadata = Metadata {hasUsedAbility :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheShadowBehindYou = TheShadowBehindYou (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theShadowBehindYou :: TreacheryCard TheShadowBehindYou
theShadowBehindYou = treachery (TheShadowBehindYou . (`with` Metadata False)) Cards.theShadowBehindYou

instance HasAbilities TheShadowBehindYou where
  getAbilities (TheShadowBehindYou (a `With` metadata)) =
    restrictedAbility a 1 OnSameLocation (ActionAbility [] $ ActionCost 1)
      : [ restrictedAbility
          a
          2
          ( InThreatAreaOf You
              <> youExist
                ( oneOf
                    [ InvestigatorWithAnyResources
                    , HandWith (HasCard $ CardWithoutKeyword Hidden)
                    ]
                )
          )
          $ forced
          $ TurnEnds #when You
        | not (hasUsedAbility metadata)
        ]

instance RunMessage TheShadowBehindYou where
  runMessage msg t@(TheShadowBehindYou (attrs `With` metadata)) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pure $ TheShadowBehindYou (attrs `with` Metadata True)
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- hidden cards can cause the then effect to fail
      hasResources <- fieldP InvestigatorResources (> 0) iid
      handCards <- field InvestigatorHand iid
      let
        hasNonHiddenCards = any (notElem Hidden . toKeywords) handCards
        hasHiddenCards = any (elem Hidden . toKeywords) handCards

      chooseOrRunOne iid
        $ [ Label
            "Discard all cards in your hand"
            $ DiscardHand iid (toSource attrs)
            : [Msg.toDiscardBy iid (attrs.ability 2) attrs | not hasHiddenCards]
          | hasNonHiddenCards
          ]
        <> [ Label
            "Lose all resources"
            [LoseAllResources iid, Msg.toDiscardBy iid (attrs.ability 2) attrs]
           | hasResources
           ]
      pure t
    EndTurn iid | treacheryInThreatArea iid attrs -> do
      pure $ TheShadowBehindYou (attrs `with` Metadata False)
    _ -> TheShadowBehindYou . (`with` metadata) <$> liftRunMessage msg attrs
