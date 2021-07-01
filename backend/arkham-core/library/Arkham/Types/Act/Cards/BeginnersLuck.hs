module Arkham.Types.Act.Cards.BeginnersLuck
  ( BeginnersLuck(..)
  , beginnersLuck
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window

newtype BeginnersLuck = BeginnersLuck ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

beginnersLuck :: BeginnersLuck
beginnersLuck = BeginnersLuck $ baseAttrs
  "02066"
  "Beginner's Luck"
  (Act 1 A)
  (Just $ RequiredClues (PerPlayer 4) Nothing)

ability :: ActAttrs -> Ability
ability attrs = (mkAbility (toSource attrs) 1 (ReactionAbility Free))
  { abilityLimit = GroupLimit PerRound 1
  }

instance ActionRunner env => HasActions env BeginnersLuck where
  getActions iid (WhenRevealToken You _) (BeginnersLuck x) =
    pure [ActivateCardAbilityAction iid (ability x)]
  getActions iid window (BeginnersLuck x) = getActions iid window x

instance
  ( ActAttrsRunner env
  , HasList Token env ()
  , HasCount SpendableClueCount env ()
  )
  => RunMessage env BeginnersLuck where
  runMessage msg a@(BeginnersLuck attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      lid <- getRandom
      a <$ unshiftMessages
        [ PlaceLocation "02074" lid
        , DiscardEncounterUntilFirst
          (toSource attrs)
          (CardMatchByType (EnemyType, singleton Criminal))
        , NextAct aid "02067"
        ]
    RequestedEncounterCard source (Just ec) | isSource attrs source -> do
      darkenedHallId <- fromJustNote "missing darkened hall"
        <$> getId (LocationWithTitle "Darkened Hall")
      a <$ unshiftMessage (SpawnEnemyAt (EncounterCard ec) darkenedHallId)
    UseCardAbility iid source Nothing 1 payments | isSource attrs source -> do
      tokensInBag <- getList @Token ()
      a <$ unshiftMessages
        [ FocusTokens tokensInBag
        , chooseOne
          iid
          [ TargetLabel
              (TokenFaceTarget token)
              [ UseCardAbility
                  iid
                  source
                  (Just . TargetMetadata $ TokenFaceTarget token)
                  1
                  payments
              ]
          | token <- tokensInBag
          ]
        ]
    UseCardAbility iid source (Just (TargetMetadata (TokenFaceTarget token))) 1 _
      | isSource attrs source
      -> do
        replaceToken token
        a <$ unshiftMessages [FocusTokens [token], Remember $ Cheated iid]
    After (GainClues _ _) -> do
      totalClues <- unSpendableClueCount <$> getCount ()
      requiredClues <- getPlayerCountValue (PerPlayer 4)
      a <$ when
        (totalClues >= requiredClues)
        (unshiftMessage $ AdvanceAct actId (toSource attrs))
    _ -> BeginnersLuck <$> runMessage msg attrs
