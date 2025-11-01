module Arkham.Act.Cards.SearchForTheManuscript (searchForTheManuscript) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement

newtype SearchForTheManuscript = SearchForTheManuscript ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheManuscript :: ActCard SearchForTheManuscript
searchForTheManuscript = act (1, A) SearchForTheManuscript Cards.searchForTheManuscript Nothing

instance HasAbilities SearchForTheManuscript where
  getAbilities = actAbilities \a ->
    [ restricted
        a
        1
        ( oneOf
            [ InvestigatorsHaveClues (AtLeast $ PerPlayer 4)
            , exists (StoryWithModifier $ ScenarioModifier "cultHasEnoughClues")
            ]
        )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage SearchForTheManuscript where
  runMessage msg a@(SearchForTheManuscript attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advanceVia #other attrs (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      cultists <- select $ NonWeaknessEnemy <> #cultist <> not_ (EnemyWithPlacement InTheShadows)
      lead <- getLead
      for_ cultists (resolveConcealed lead)
      shuffleSetAsideIntoEncounterDeck
        $ mapOneOf cardIs [Enemies.umbralHarbinger, Enemies.emissaryFromYuggoth]
      shuffleEncounterDiscardBackIn
      advanceActDeck attrs
      pure a
    _ -> SearchForTheManuscript <$> liftRunMessage msg attrs
