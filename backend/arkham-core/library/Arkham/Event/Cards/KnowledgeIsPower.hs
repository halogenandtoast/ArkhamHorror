module Arkham.Event.Cards.KnowledgeIsPower
  ( knowledgeIsPower
  , KnowledgeIsPower(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Card
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Projection
import Arkham.Trait ( Trait (Spell, Tome) )

newtype KnowledgeIsPower = KnowledgeIsPower EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knowledgeIsPower :: EventCard KnowledgeIsPower
knowledgeIsPower = event KnowledgeIsPower Cards.knowledgeIsPower

instance RunMessage KnowledgeIsPower where
  runMessage msg e@(KnowledgeIsPower attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      assets <-
        selectList
        $ assetControlledBy iid
        <> AssetOneOf [AssetWithTrait Spell, AssetWithTrait Tome]
        <> AssetWithPerformableAbility
             (AbilityOneOf [AbilityIsActionAbility, AbilityIsFastAbility])
             [IgnoreAllCosts]

      cards <- fieldMapM
        InvestigatorHand
        (filterM
          (`extendedCardMatch` (BasicCardMatch
                                   (CardWithOneOf
                                       [CardWithTrait Tome, CardWithTrait Spell]
                                   <> CardWithType AssetType
                                   )
                               <> CardWithPerformableAbility
                                    (AbilityOneOf
                                      [ AbilityIsActionAbility
                                      , AbilityIsFastAbility
                                      ]
                                    )
                                    [IgnoreAllCosts]
                               )
          )
        )
        iid

      push $ chooseOne iid $ [targetLabel asset [] | asset <- assets]
        <> [targetLabel (toCardId card) [] | card <- cards]
      pure e
    _ -> KnowledgeIsPower <$> runMessage msg attrs
