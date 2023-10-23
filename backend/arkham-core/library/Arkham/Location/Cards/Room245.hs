module Arkham.Location.Cards.Room245 (
  room245,
  Room245 (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Name
import Arkham.Projection

newtype Room245 = Room245 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

room245 :: LocationCard Room245
room245 = locationWith Room245 Cards.room245 0 (Static 0) (labelL .~ "room245")

instance HasAbilities Room245 where
  getAbilities (Room245 attrs) =
    withRevealedAbilities attrs [restrictedAbility attrs 1 Here actionAbility]

instance RunMessage Room245 where
  runMessage msg l@(Room245 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mTopOfDiscard <- fieldMap InvestigatorDiscard headMay iid
      investigators <- getInvestigators
      push $ beginSkillTest iid (toAbilitySource attrs 1) iid #intellect 3
      case mTopOfDiscard of
        Just topOfDiscard -> do
          push
            $ skillTestModifier
              (toAbilitySource attrs 1)
              (toCardId topOfDiscard)
              PlaceOnBottomOfDeckInsteadOfDiscard
          for_ investigators $ \investigator -> do
            if investigator == iid
              then
                push
                  $ skillTestModifiers
                    (toAbilitySource attrs 1)
                    investigator
                    [ CanCommitToSkillTestsAsIfInHand $ toCard topOfDiscard
                    , CannotCommitCards (NotCard $ CardWithId $ toCardId topOfDiscard)
                    ]
              else
                push
                  $ skillTestModifiers
                    (toAbilitySource attrs 1)
                    investigator
                    [ CannotCommitCards AnyCard
                    ]
        Nothing -> do
          for_ investigators $ \investigator -> do
            push
              $ skillTestModifiers
                (toAbilitySource attrs 1)
                investigator
                [ CannotCommitCards AnyCard
                ]

      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      player <- getPlayer iid
      iids <- selectWithField InvestigatorClues $ investigatorAt (toId attrs) <> InvestigatorWithAnyClues
      timeWornLocket <- selectOne $ assetIs Assets.timeWornLocket

      unless (null iids || isNothing timeWornLocket) $ do
        named <- traverse (\(iid', n) -> (,n) <$> field InvestigatorName iid') iids
        push
          $ chooseAmounts
            player
            "number of clues to move to Time-worn Locket"
            (MinAmountTarget 0)
            (map (\(name, n) -> (toTitle name, (0, n))) named)
            (toTarget attrs)
      pure l
    ResolveAmounts _ choices (isTarget attrs -> True) -> do
      named <- selectWithField InvestigatorName UneliminatedInvestigator
      let
        iidsWithAmounts =
          flip mapMaybe named $ \(iid', name) ->
            let amount = getChoiceAmount (toTitle name) choices
             in guard (amount > 0) $> (iid', amount)
      timeWornLocket <- selectJust $ assetIs Assets.timeWornLocket
      pushAll
        $ [ MovedClues (toSource iid) (toTarget timeWornLocket) n
          | (iid, n) <- iidsWithAmounts
          ]
      pure l
    _ -> Room245 <$> runMessage msg attrs
