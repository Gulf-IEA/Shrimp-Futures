---
name: Stakeholder Analysis Field Trip Checklist Template
about: This template is to help plan and organize winter 2025 field trips for the stakeholder mapping and analysis milestone.
title: Field Trip Checklist [field trip name here]
labels: ''
assignees: ''

---

body:
- type: checkboxes
  id: states
  attributes:
    label: What state(s) will be visited on this trip?
    options:
      - North Carolina
      - South Carolina
      - Georgia
      - Florida
      - Alabama
      - Mississippi
      - Louisiana
      - Texas
  validations:
    required: true
  
- type: text
  id: regions
  attributes:
    label: What specific areas will be visited?
    placeholder: "These are the specific regions or cities within each state that you plan on visiting."
  validations:
    required: true

- type: checkboxes
  id: personnel
  attributes:
    label: Who will be going on this trip?
    options:
      - Carissa Gervasi
      - Kelsi Furman
      - Matt McPherson
      - Other
  validations:
    required: true

- type: text
  id: other_personnel
  attributes:
    label: If "Other," please specify:
  conditions:
    - field: personnel
      value: Other

