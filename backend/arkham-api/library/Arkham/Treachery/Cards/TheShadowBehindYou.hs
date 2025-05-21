module Arkham.Treachery.Cards.TheShadowBehindYou (theShadowBehindYou) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Investigator.Types
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
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
    restricted a 1 OnSameLocation actionAbility
      : [ restricted
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
      whenNone (treacheryInThreatAreaOf iid <> treacheryIs Cards.theShadowBehindYou) do
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

      chooseOrRunOneM iid $ withI18n do
        when hasNonHiddenCards do
          labeled' "discardAllCardsFromHand" do
            push $ DiscardHand iid (toSource attrs)
            unless hasHiddenCards $ toDiscardBy iid (attrs.ability 2) attrs

        when hasResources do
          labeled' "loseAllResources" do
            push $ LoseAllResources iid (attrs.ability 1)
            toDiscardBy iid (attrs.ability 2) attrs
      pure t
    EndTurn iid | treacheryInThreatArea iid attrs -> do
      pure $ TheShadowBehindYou (attrs `with` Metadata False)
    _ -> TheShadowBehindYou . (`with` metadata) <$> liftRunMessage msg attrs
