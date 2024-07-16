module Arkham.Event.Cards.Refine (refine, Refine (..)) where

import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Customization
import Arkham.Matcher
import Arkham.Name
import Arkham.PlayerCard
import Data.Function (on)
import Data.List (nubBy)

newtype Refine = Refine EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

refine :: EventCard Refine
refine = event Refine Cards.refine

instance RunMessage Refine where
  runMessage msg e@(Refine attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <- select $ OwnedBy (InvestigatorWithId iid) <> basic CardWithAvailableCustomization
      let cards' = nubBy ((==) `on` toCardCode) cards
      focusCards cards' $ chooseOneToHandleWith iid attrs cards'
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (CardIdTarget cid) -> do
      card <- getCard cid
      case card of
        PlayerCard pc -> do
          let customizations = cdCustomizations $ toCardDef card
          let availableCustomizations =
                filter
                  (not . hasCustomization_ customizations (pcCustomizations pc))
                  (keys customizations)
          chooseOne
            iid
            [ Label
              (tshow customization)
              [ForTarget (toTarget attrs) $ IncreaseCustomization iid (toCardCode card) customization []]
            | customization <- availableCustomizations
            ]
        _ -> pure ()
      pure e
    ForTarget (isTarget attrs -> True) msg'@(IncreaseCustomization iid cardCode customization choices) -> do
      mcard <- selectOne $ OwnedBy (InvestigatorWithId iid) <> basic (CardWithCardCode cardCode)
      for_ mcard \card -> do
        let requiredChoices = choicesRequired customization
        let remaining = cardRemainingCheckMarks card customization
        if remaining == Just 1 && length choices < length requiredChoices
          then case requiredChoices of
            (x : _) -> case x of
              CustomizationTraitChoice -> do
                chooseOneDropDown
                  iid
                  [ ( tshow trait
                    , ForTarget (toTarget attrs)
                        $ IncreaseCustomization iid cardCode customization (ChosenTrait trait : choices)
                    )
                  | trait <- [minBound ..]
                  ]
              CustomizationCardChoice matcher -> do
                chooseOneDropDown
                  iid
                  [ ( c
                    , ForTarget (toTarget attrs)
                        $ IncreaseCustomization iid cardCode customization (ChosenCard c : choices)
                    )
                  | c <-
                      sort
                        $ nub
                        $ map toTitle
                        $ filter ((`cardMatch` matcher) . (`lookupPlayerCard` nullCardId)) (toList allPlayerCards)
                  ]
              CustomizationSkillChoice -> do
                chooseOneDropDown
                  iid
                  [ ( tshow skill
                    , ForTarget (toTarget attrs)
                        $ IncreaseCustomization iid cardCode customization (ChosenSkill skill : choices)
                    )
                  | skill <- [minBound ..]
                  ]
              CustomizationIndexChoice zs -> do
                chooseOneDropDown
                  iid
                  [ ( tshow z
                    , ForTarget (toTarget attrs)
                        $ IncreaseCustomization iid cardCode customization (ChosenIndex n : choices)
                    )
                  | (n, z) <- withIndex zs
                  ]
            _ -> error "expected exactly one required choice"
          else push msg'

      pure e
    _ -> Refine <$> liftRunMessage msg attrs
