module Arkham.Event.Cards.KnowledgeIsPower (
  knowledgeIsPower,
  KnowledgeIsPower (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Helpers.Card
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Spell, Tome))
import Arkham.Window qualified as Window

newtype KnowledgeIsPower = KnowledgeIsPower EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knowledgeIsPower :: EventCard KnowledgeIsPower
knowledgeIsPower = event KnowledgeIsPower Cards.knowledgeIsPower

cardMatcher :: ExtendedCardMatcher
cardMatcher =
  basic
    ( oneOf [#tome, #spell]
        <> #asset
    )
    <> CardWithPerformableAbility
      (AbilityOneOf [AbilityIsActionAbility, AbilityIsFastAbility])
      [IgnoreAllCosts]

instance RunMessage KnowledgeIsPower where
  runMessage msg e@(KnowledgeIsPower attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      assets <-
        select
          $ assetControlledBy iid
          <> AssetOneOf [AssetWithTrait Spell, AssetWithTrait Tome]
          <> AssetWithPerformableAbility
            (AbilityOneOf [AbilityIsActionAbility, AbilityIsFastAbility])
            [IgnoreAllCosts]

      cards <-
        fieldMapM
          InvestigatorHand
          (filterM (`extendedCardMatch` cardMatcher))
          iid

      canDraw <- iid <=~> InvestigatorCanDrawCards Anyone
      drawing <- drawCards iid attrs 1
      player <- getPlayer iid

      push
        $ chooseOne player
        $ [ targetLabel asset [HandleTargetChoice iid (toSource attrs) (AssetTarget asset)]
          | asset <- assets
          ]
        <> [ targetLabel (toCardId card)
            $ [ AddCardEntity card
              , HandleTargetChoice
                  iid
                  (toSource attrs)
                  (AssetTarget $ AssetId $ unsafeCardIdToUUID $ toCardId card)
              , RemoveCardEntity card
              ]
            <> [ chooseOne
                player
                [ Label
                    "Discard to draw 1 card"
                    [ DiscardCard iid (toSource attrs) (toCardId card)
                    , drawing
                    ]
                , Label "Do not discard" []
                ]
               | canDraw
               ]
           | card <- cards
           ]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      let
        adjustAbility ab =
          applyAbilityModifiers
            (ab {abilityDoesNotProvokeAttacksOfOpportunity = True})
            [IgnoreAllCosts]
      abilities <-
        selectMap adjustAbility
          $ AssetAbility (AssetWithId aid)
          <> AbilityOneOf [AbilityIsActionAbility, AbilityIsFastAbility]
      abilities' <-
        filterM
          ( \ab ->
              anyM
                (\w -> getCanPerformAbility iid w ab)
                (Window.defaultWindows iid)
          )
          abilities
      player <- getPlayer iid
      push $ chooseOne player [AbilityLabel iid ab [] [] | ab <- abilities']
      pure e
    _ -> KnowledgeIsPower <$> runMessage msg attrs
