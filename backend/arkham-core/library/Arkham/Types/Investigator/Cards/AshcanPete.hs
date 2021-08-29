module Arkham.Types.Investigator.Cards.AshcanPete
  ( AshcanPete(..)
  , ashcanPete
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Assets
import Arkham.Types.Ability
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Matcher hiding (FastPlayerWindow)
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait

newtype AshcanPete = AshcanPete InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
  deriving newtype (Show, ToJSON, FromJSON, Entity)

ashcanPete :: AshcanPete
ashcanPete = AshcanPete $ base & startsWithL .~ [Assets.duke]
 where
  base = baseAttrs
    "02005"
    "\"Ashcan\" Pete"
    Survivor
    Stats
      { health = 6
      , sanity = 5
      , willpower = 4
      , intellect = 2
      , combat = 2
      , agility = 3
      }
    [Drifter]

instance HasAbilities env AshcanPete where
  getAbilities _ _ (AshcanPete x) = pure
    [ restrictedAbility
        x
        1
        (Self <> AssetExists (AssetOwnedBy You <> AssetExhausted))
      $ FastAbility
      $ HandDiscardCost 1 Nothing mempty mempty
    ]

instance HasTokenValue env AshcanPete where
  getTokenValue (AshcanPete attrs) iid ElderSign | iid == toId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue (AshcanPete attrs) iid token = getTokenValue attrs iid token

instance InvestigatorRunner env => RunMessage env AshcanPete where
  runMessage msg i@(AshcanPete attrs) = case msg of
    ResolveToken _drawnToken ElderSign iid | iid == toId attrs ->
      i <$ push (Ready $ CardCodeTarget "02014")
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      targets <- selectListMap AssetTarget (AssetOwnedBy You <> AssetExhausted)
      i <$ push (chooseOne (toId attrs) [ Ready target | target <- targets ])
    _ -> AshcanPete <$> runMessage msg attrs
