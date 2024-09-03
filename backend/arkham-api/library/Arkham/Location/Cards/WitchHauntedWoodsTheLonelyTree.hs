module Arkham.Location.Cards.WitchHauntedWoodsTheLonelyTree (
  witchHauntedWoodsTheLonelyTree,
  WitchHauntedWoodsTheLonelyTree (..),
) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Discard
import Arkham.Draw.Types
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype WitchHauntedWoodsTheLonelyTree = WitchHauntedWoodsTheLonelyTree LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchHauntedWoodsTheLonelyTree :: LocationCard WitchHauntedWoodsTheLonelyTree
witchHauntedWoodsTheLonelyTree =
  location WitchHauntedWoodsTheLonelyTree Cards.witchHauntedWoodsTheLonelyTree 2 (PerPlayer 1)

instance HasModifiersFor WitchHauntedWoodsTheLonelyTree where
  getModifiersFor (InvestigatorTarget iid) (WitchHauntedWoodsTheLonelyTree a) =
    do
      handLength <- fieldMap InvestigatorHand length iid
      pure $ toModifiers a [CannotInvestigateLocation (toId a) | handLength >= 3 && handLength <= 5]
  getModifiersFor _ _ = pure []

instance HasAbilities WitchHauntedWoodsTheLonelyTree where
  getAbilities (WitchHauntedWoodsTheLonelyTree a) =
    withBaseAbilities
      a
      [ playerLimit PerRound
          $ restrictedAbility
            a
            1
            ( Here
                <> exists
                  ( oneOf [You, at_ (NotLocation YourLocation <> "Witch-Haunted Woods")]
                      <> InvestigatorWithDiscardableCard
                  )
            )
          $ FastAbility Free
      ]

instance RunMessage WitchHauntedWoodsTheLonelyTree where
  runMessage msg l@(WitchHauntedWoodsTheLonelyTree attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      handLength <- fieldMap InvestigatorHand length iid
      canDraw <- can.draw.cards FromOtherSource iid

      iidsForDraw <-
        select @InvestigatorMatcher
          $ can.draw.cards FromOtherSource
          $ InvestigatorAt ("Witch-Haunted Woods" <> not_ (locationWithInvestigator iid))

      iidsForDiscard <-
        select
          $ InvestigatorWithDiscardableCard
          <> at_ ("Witch-Haunted Woods" <> not_ (locationWithInvestigator iid))

      chooseOtherDraw <- for iidsForDraw $ \other -> do
        let drawing = newCardDraw attrs other 1
        pure $ targetLabel other [DrawCards other drawing]

      let drawing = newCardDraw attrs iid 1
      player <- getPlayer iid

      push
        $ chooseOrRunOne player
        $ [ Label
            "You choose and discard 1 card from your hand, then an investigator at a different Witch-Haunted Woods draws 1 card"
            [ toMessage
                $ (chooseAndDiscardCard iid attrs)
                  { discardThen = guard (notNull chooseOtherDraw) $> chooseOrRunOne player chooseOtherDraw
                  }
            ]
          | handLength > 0
          ]
        <> [ Label
            "vice versa"
            [ chooseOrRunOne
                player
                [ targetLabel
                  other
                  [toMessage $ (chooseAndDiscardCard other attrs) {discardThen = Just $ DrawCards iid drawing}]
                | other <- iidsForDiscard
                ]
            ]
           | notNull iidsForDiscard && canDraw
           ]
      pure l
    _ -> WitchHauntedWoodsTheLonelyTree <$> runMessage msg attrs
