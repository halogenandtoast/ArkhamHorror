module Arkham.Asset.Cards.ProfessorWilliamWebbFinderOfHiddenConnections2 (
  professorWilliamWebbFinderOfHiddenConnections2,
  ProfessorWilliamWebbFinderOfHiddenConnections2 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Discover
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Modifier

newtype ProfessorWilliamWebbFinderOfHiddenConnections2
  = ProfessorWilliamWebbFinderOfHiddenConnections2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

professorWilliamWebbFinderOfHiddenConnections2
  :: AssetCard ProfessorWilliamWebbFinderOfHiddenConnections2
professorWilliamWebbFinderOfHiddenConnections2 =
  ally
    ProfessorWilliamWebbFinderOfHiddenConnections2
    Cards.professorWilliamWebbFinderOfHiddenConnections2
    (1, 2)

instance HasAbilities ProfessorWilliamWebbFinderOfHiddenConnections2 where
  getAbilities (ProfessorWilliamWebbFinderOfHiddenConnections2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (SuccessfulInvestigation #when You Anywhere) (exhaust a <> assetUseCost a Secret 1)
    ]

instance RunMessage ProfessorWilliamWebbFinderOfHiddenConnections2 where
  runMessage msg a@(ProfessorWilliamWebbFinderOfHiddenConnections2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canGetItem <- can.have.cards.leaveDiscard iid
      items <- select $ #item <> inDiscardOf iid
      when (notNull items && canGetItem) do
        focusCards items \unfocus -> do
          chooseOne iid [targetLabel item [unfocus, AddToHand iid [item]] | item <- items]

      hasLocations <-
        selectAny $ ConnectedFrom (locationWithInvestigator iid) <> locationWithDiscoverableCluesBy iid

      when hasLocations do
        chooseOne
          iid
          [ Label "Discover clue at your location" []
          , Label "Discover a clue at a connecting location" [DoStep 1 msg]
          ]

      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      -- We redirect success to this, but do nothing since the ability handles it
      withLocationOf iid \lid ->
        skillTestModifier
          (attrs.ability 1)
          (toTarget lid)
          (AlternateSuccessfullInvestigation (toTarget attrs))
      locations <-
        select $ ConnectedFrom (locationWithInvestigator iid) <> locationWithDiscoverableCluesBy iid
      chooseOne
        iid
        [ targetLabel location [DiscoverClues iid $ discover location (attrs.ability 1) 1]
        | location <- locations
        ]
      pure a
    _ -> ProfessorWilliamWebbFinderOfHiddenConnections2 <$> lift (runMessage msg attrs)
